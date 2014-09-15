/*
Copyright 2014 sumito3478 <sumito3478@gmail.com>
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package scarango
package internal

import scala.concurrent._
import java.util.{ concurrent => juc }

private[scarango] object Implicits {
  // Use global ExecutionContext instead of passing E.C.s via implicit params
  // See http://blog.jessitron.com/2014/02/scala-global-executioncontext-makes.html
  implicit val executionContext = ExecutionContext.Implicits.global
}

