package stapl.core

import stapl.core.typeclasses.Instances
import stapl.core.syntax.Syntax

package object dsl extends DSL with Instances with Syntax {
  type Subject = stapl.core.Subject
  type Resource = stapl.core.Resource
  type Action = stapl.core.Action
  type Environment = stapl.core.Environment
}