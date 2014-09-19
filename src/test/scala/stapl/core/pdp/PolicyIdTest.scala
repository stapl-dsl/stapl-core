/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package stapl.core.pdp

import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit
import stapl.core._

object PolicyIdTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
/**
 * Some tests to see whether policy ids are generated correctly.
 */
class PolicyIdTest extends AssertionsForJUnit{

  
  @Before def setup() {
    // nothing to do
  }

  @Test def testFqidOfRule() {
    val rule1 = Rule("rule1") := deny
    
    assertEquals("rule1", rule1.fqid)
  }
  
  @Test def testFqidOfSimplePolicy() {
    val rule1 = Rule("rule1") := deny
    val rule2 = Rule("rule2") := deny
    
    val policy1 = 
      Policy("policy1") := apply PermitOverrides to (
        rule1,
        rule2
      )
    
    assertEquals("policy1", policy1.fqid)
  }
  
  @Test def testFqidOfRulesInSimplePolicy() {
    val rule1 = Rule("rule1") := deny
    val rule2 = Rule("rule2") := deny
    
    val policy1 = 
      Policy("policy1") := apply PermitOverrides to (
        rule1,
        rule2
      )
    
    assertEquals("policy1>rule1", rule1.fqid)
    assertEquals("policy1>rule2", rule2.fqid)
  }
  
  @Test def testFqidChangesIfneeded() {
    val rule1 = Rule("rule1") := deny
    val rule2 = Rule("rule2") := deny
    
    assertEquals("rule1", rule1.fqid)
    assertEquals("rule2", rule2.fqid)
    
    val policy1 = 
      Policy("policy1") := apply PermitOverrides to (
        rule1,
        rule2
      )
    
    assertEquals("policy1", policy1.fqid)
    assertEquals("policy1>rule1", rule1.fqid)
    assertEquals("policy1>rule2", rule2.fqid)
  }

}