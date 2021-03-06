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
package io.gatling.core.result.writer

import io.gatling.core.result.message.{ MessageEvent, Status }
import io.gatling.core.session.GroupBlock

sealed trait DataWriterMessage

case class ShortScenarioDescription(name: String, nbUsers: Int)

case class Init(runMessage: RunMessage, scenarios: Seq[ShortScenarioDescription]) extends DataWriterMessage

case object Terminate extends DataWriterMessage

case class RequestMessage(
    scenario: String,
    userId: String,
    groupHierarchy: List[String],
    name: String,
    requestStartDate: Long,
    requestEndDate: Long,
    responseStartDate: Long,
    responseEndDate: Long,
    status: Status,
    message: Option[String],
    extraInfo: List[Any]) extends DataWriterMessage {

  def responseTime = responseEndDate - requestStartDate
}

case class RunMessage(
  simulationClassName: String,
  simulationId: String,
  start: Long,
  runDescription: String)
    extends DataWriterMessage {

  val runId = simulationId + "-" + start
}

case class UserMessage(
  scenarioName: String,
  userId: String,
  event: MessageEvent,
  startDate: Long,
  endDate: Long)
    extends DataWriterMessage

case class GroupMessage(
  scenarioName: String,
  userId: String,
  group: GroupBlock,
  groupHierarchy: List[String],
  startDate: Long,
  endDate: Long,
  status: Status)
    extends DataWriterMessage
