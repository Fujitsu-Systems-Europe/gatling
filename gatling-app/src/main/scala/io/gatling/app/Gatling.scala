/**
 * Copyright 2011-2014 eBusiness Information, Groupe Excilys (www.ebusinessinformation.fr)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.app

import java.lang.System.currentTimeMillis

import scala.Console.err
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{ Failure, Success, Try }

import scopt.OptionParser

import io.gatling.app.CommandLineConstants._
import io.gatling.charts.report.ReportsGenerator
import io.gatling.core.assertion.AssertionValidator
import io.gatling.core.config.{ GatlingFiles, GatlingPropertiesBuilder }
import io.gatling.core.config.GatlingConfiguration
import io.gatling.core.config.GatlingConfiguration.configuration
import io.gatling.core.result.reader.DataReader
import io.gatling.core.runner.{ Runner, Selection }
import io.gatling.core.scenario.Simulation
import io.gatling.core.util.StringHelper
import io.gatling.core.util.StringHelper.RichString

/**
 * Object containing entry point of application
 */
object Gatling {

  def main(args: Array[String]): Unit = sys.exit(runGatling(args, None))

  def runGatling(args: Array[String], simulationClass: Option[Class[Simulation]]): Int = {
    val props = new GatlingPropertiesBuilder

    val cliOptsParser = new OptionParser[Unit]("gatling") with CommandLineConstantsSupport[Unit] {
      help(Help).text("Show help (this message) and exit")
      opt[Unit](NoReports).foreach(_ => props.noReports()).text("Runs simulation but does not generate reports")
      opt[Unit](Mute).foreach(_ => props.mute()).text("Runs in mute mode: don't asks for run description nor simulation ID, use defaults").hidden()
      opt[String](ReportsOnly).foreach(props.reportsOnly).valueName("<directoryName>").text("Generates the reports for the simulation in <directoryName>")
      opt[String](DataFolder).foreach(props.dataDirectory).valueName("<directoryPath>").text("Uses <directoryPath> as the absolute path of the directory where feeders are stored")
      opt[String](ResultsFolder).foreach(props.resultsDirectory).valueName("<directoryPath>").text("Uses <directoryPath> as the absolute path of the directory where results are stored")
      opt[String](RequestBodiesFolder).foreach(props.requestBodiesDirectory).valueName("<directoryPath>").text("Uses <directoryPath> as the absolute path of the directory where request bodies are stored")
      opt[String](SimulationsFolder).foreach(props.sourcesDirectory).valueName("<directoryPath>").text("Uses <directoryPath> to discover simulations that could be run")
      opt[String](Simulation).foreach(props.simulationClass).valueName("<className>").text("Runs <className> simulation")
      opt[String](OutputDirectoryBaseName).foreach(props.outputDirectoryBaseName).valueName("<name>").text("Use <name> for the base name of the output directory")
      opt[String](SimulationDescription).foreach(props.runDescription).valueName("<description>").text("A short <description> of the run to include in the report")
    }

    // if arguments are incorrect, usage message is displayed
    if (cliOptsParser.parse(args)) new Gatling(props.build, simulationClass).start
    else GatlingStatusCodes.InvalidArguments
  }
}
class Gatling(props: mutable.Map[String, _], simulationClass: Option[Class[Simulation]]) {

  type SelectedSingleSimulation = Option[Class[Simulation]]
  type AllSimulations = List[Class[Simulation]]

  def start: Int = {
    StringHelper.checkSupportedJavaVersion()
    GatlingConfiguration.setUp(props)

    val simulations = loadSimulations
    val singleSimulation = selectSingleSimulationIfPossible(simulations)

    val runId = runSimulationIfNecessary(singleSimulation, simulations)

    val dataReader = DataReader.newInstance(runId)

    if (!configuration.charting.noReports) generateReports(runId, dataReader)

    if (dataReader.assertions.nonEmpty) applyAssertions(dataReader)
    else GatlingStatusCodes.Success
  }

  private def loadSimulations = {
    simulationClass.flatMap(_ => configuration.core.directory.reportsOnly) match {
      case Some(_) => Nil
      case None    => SimulationClassLoader(GatlingFiles.binariesDirectory).simulationClasses.sortBy(_.getName)
    }
  }

  private def selectSingleSimulationIfPossible(simulations: AllSimulations): SelectedSingleSimulation = {
      def singleSimulationFromConfig =
        configuration.core.simulationClass flatMap { className =>
          simulations.find(_.getCanonicalName == className) match {
            case Some(simulation) => Some(simulation)
            case None =>
              err.println(s"The requested class('$className') can not be found in the classpath or does not extends Simulation.")
              None
          }
        }

      def singleSimulationFromList = {
        val firstSimulation = simulations.headOption
        firstSimulation.foreach(simulation => println(s"${simulation.getName} is the only simulation, executing it."))
        firstSimulation
      }

    simulationClass orElse singleSimulationFromConfig orElse singleSimulationFromList
  }

  private def runSimulationIfNecessary(singleSimulation: SelectedSingleSimulation, simulations: AllSimulations): String = {
    configuration.core.directory.reportsOnly.getOrElse {
      // -- If no single simulation was available, allow user to select one -- //
      val simulation = singleSimulation.getOrElse(interactiveSelect(simulations))

      // -- Ask for simulation ID and run description if required -- //
      val muteModeActive = configuration.core.muteMode
      val defaultBaseName = defaultOutputDirectoryBaseName(simulation)
      val simulationId = if (muteModeActive) defaultBaseName else askSimulationId(simulation, defaultBaseName)
      val runDescription = if (muteModeActive) "" else askRunDescription()

      // -- Run Gatling -- //
      val selection = Selection(simulation, simulationId, runDescription)
      new Runner(selection).run
    }
  }

  private def askSimulationId(clazz: Class[Simulation], defaultBaseName: String): String = {
      @tailrec
      def loop(defaultSimulationId: String): String = {
        println(s"Select simulation id (default is '$defaultSimulationId'). Accepted characters are a-z, A-Z, 0-9, - and _")
        val input = StdIn.readLine().trim
        if (input.matches("[\\w-_]*")) input
        else {
          println(s"$input contains illegal characters")
          loop(defaultSimulationId)
        }
      }

    val input = loop(defaultBaseName)
    if (input.nonEmpty) input else defaultBaseName
  }

  private def askRunDescription(): String = {
    println("Select run description (optional)")
    StdIn.readLine().trim
  }

  def interactiveSelect(simulations: AllSimulations): Class[Simulation] = {
      @tailrec
      def readSimulationNumber(validRange: Range): Int = {
        println("Choose a simulation number:")
        for ((simulation, index) <- simulations.zipWithIndex) {
          println(s"     [$index] ${simulation.getName}")
        }

        Try(StdIn.readInt()) match {
          case Failure(_) =>
            println("Invalid characters, please provide a correct simulation number:")
            readSimulationNumber(validRange)
          case Success(number) =>
            if (validRange contains number) number
            else {
              println(s"Invalid selection, must be in $validRange")
              readSimulationNumber(validRange)
            }
        }
      }

    if (simulations.isEmpty) {
      println("There is no simulation script. Please check that your scripts are in user-files/simulations")
      sys.exit()
    }
    simulations(readSimulationNumber(0 until simulations.size))
  }

  private def generateReports(outputDirectoryName: String, dataReader: DataReader): Unit = {
    println("Generating reports...")
    val start = currentTimeMillis
    val indexFile = ReportsGenerator.generateFor(outputDirectoryName, dataReader)
    println(s"Reports generated in ${(currentTimeMillis - start) / 1000}s.")
    println(s"Please open the following file: ${indexFile.toFile}")
  }

  private def applyAssertions(dataReader: DataReader) =
    if (AssertionValidator.validateAssertions(dataReader)) {
      println("Simulation successful.")
      GatlingStatusCodes.Success
    } else {
      println("Simulation failed.")
      GatlingStatusCodes.AssertionsFailed
    }

  private def defaultOutputDirectoryBaseName(clazz: Class[Simulation]) =
    configuration.core.outputDirectoryBaseName.getOrElse(clazz.getSimpleName.clean)
}
