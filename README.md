# STAPL

STAPL or the Simple Tree-structured Attribute-based Policy Language is a Scala DSL designed to express and evaluate XACML-like policies easily. STAPL provides a human-friendly policy syntax and an efficient evaluation engine that is easy to test and use in Scala and Java applications.

For quickly getting started with STAPL, check out the [Quick Start][1] below.

### Related projects

This repository hosts `stapl-core`. Some other projects extend the core:

* [stapl-getting-started][2] provides a simple Maven project to quickly get you started with STAPL. For more information about this, see [below][3].
* [stapl-templates](https://github.com/maartendecat/stapl-templates) provides a number of policy templates that make it easier to write policies.
* [stapl-java-api][4] provides a simple Java wrapper for the STAPL policy engine. More information [below][5].
* [stapl-examples](https://github.com/maartendecat/stapl-examples) provides examples of using STAPL, the templates and the Java API.
* [stapl-performance](https://github.com/maartendecat/stapl-performance) provides some performance tests.

# Example

Here is a simple example of a STAPL policy based on an e-health scenario:

```scala
package mypackage

import stapl.core._

object PolicyFromTheReadMe extends BasicPolicy {
    import stapl.core.dsl._
    
    subject.roles		= ListAttribute(String)
    subject.treated		= ListAttribute(String)
    resource.type_ 	    = SimpleAttribute(String)
    resource.owner_id   = SimpleAttribute(String)
    // action.id is defined by STAPL itself
     
    // Permit only if the physician treated the owner of the patient data.
    val policy = Policy("e-health example") := when ((action.id === "view") & (resource.type_ === "patient-data") 
            & ("physician" in subject.roles)) apply PermitOverrides to (
      Rule("requirement-for-permit") := permit iff (resource.owner_id in subject.treated),
      Rule("default deny") := deny
    )
}
```

Quite consice and readable, no? As comparison, here is the XACML representation of this same policy:

```xml
<Policy PolicyId="e-health example" 
        RuleCombiningAlgId="urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:permit-overrides">
  <Description>Permit only if the physician treated the owner of the patient data.</Description>
  <Target>
    <Actions>
      <Action>
        <ActionMatch MatchId="urn:oasis:names:tc:xacml:1.0:function:string-equal">
          <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">view</AttributeValue>
	        <ActionAttributeDesignator AttributeId="action:id" DataType="http://www.w3.org/2001/XMLSchema#string"/>
        </ActionMatch>
      </Action>
    </Actions>
    <Resources>
      <Resource>
        <ResourceMatch MatchId="urn:oasis:names:tc:xacml:1.0:function:string-equal">
            <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">patient-data</AttributeValue>
	        <ResourceAttributeDesignator AttributeId="resource:type" DataType="http://www.w3.org/2001/XMLSchema#string"/>
        </ResourceMatch>
      </Resource>
    </Resources>
    <Subjects>
      <Subject>
	      <SubjectMatch MatchId="urn:oasis:names:tc:xacml:1.0:function:string-equal">
	        <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">physician</AttributeValue>
		      <SubjectAttributeDesignator AttributeId="subject:roles" DataType="http://www.w3.org/2001/XMLSchema#string"/>
	      </SubjectMatch>
      </Subject>
    </Subjects>
  </Target>
  <Rule RuleId="requirement-for-permit" Effect="Permit">
    <Description>Permit if the physician treated the owner of the patient data.</Description>
    <Condition>
      <Apply FunctionId="urn:oasis:names:tc:xacml:1.0:function:string-is-in">
        <Apply FunctionId="urn:oasis:names:tc:xacml:1.0:function:string-one-and-only">
          <ResourceAttributeDesignator AttributeId="resource:owner:id" DataType="http://www.w3.org/2001/XMLSchema#string"/>
        </Apply>
        <SubjectAttributeDesignator AttributeId="subject:treated" DataType="http://www.w3.org/2001/XMLSchema#string"/>
      </Apply>
    </Condition>
  </Rule>
  <Rule RuleId="deny" Effect="Deny">
    <Description>Deny otherwise</Description>
  </Rule>
</Policy>
```

For more examples, see the [stapl-examples](https://github.com/maartendecat/stapl-examples) project.

# Features

#### Simple and concise syntax
No more need for XML copy-pasting.

#### Policy references
These allow you to build large policies modularly. If you want, you can even use the Scala package system to structure large policy sets.

#### Policy templates
These allow you increase modularity even more by encapsulating common policy patterns.

#### Run-time policy loading
STAPL policies can be compiled just as any other Scala and Java code. However, your application should not be recompiled for every policy change. Therefore, STAPL allows you to load policies at run-time. If you would like to know, this is done internally using the Scala interpreter.

#### Efficient policy evaluation
Compared to [our optimized version of the SunXACML engine][6], STAPL policies can be evaluated multiple times faster than their XACML equivalents.

#### Extensive logging
Wondering why a certain decision is Permit or Deny? Turn on logging and get an overview of the whole policy evaluation process.

### Future-plans

Towards the future, we plan to add the following features.

#### Stand-alone deployment
The STAPL evaluation engine can be compiled directly into your Scala or Java application. However, for other languages, we plan to allow the engine to be deployed stand-alone and reached using [Apache Thrift][7].



# Employing the engine in Scala

To get an access control decision, simply load the policy in the policy decision point (PDP) and ask for a decision:

```scala
import mypackage.PolicyFromTheReadMe.policy
val pdp = new PDP(policy)
pdp.evaluate("john", "view", "doc123",
    subject.roles -> List("physician"),
    subject.treated -> List("patient1", "patient2"),
    resource.type_ -> "patient-data",
    resource.owner_id -> "patient3") // so this will result in a Deny
}
```

If you cannot provide all attributes with the decision request or want to dynamically fetch attributes from a database, write your own AttributeFinderModule and add it to the PDP:

```scala
class MyAttributeFinderModule extends AttributeFinderModule {
    
    override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType):
        // fetch the attribute from the database and return it
        ...
}

val finder = new AttributeFinder
finder += new MyAttributeFinderModule
val pdp = new PDP(policy, finder)
```

# Employing the engine in Java

Because STAPL is implemented in Scala, you're best off with writing your policies in Scala. However, the policy evaluation engine can easily be used from Java as well. In theory, Scala classes can directly be used in Java. In practice however, this leads to some annoying boiler-plate code. Therefore we also provide a simple API that wraps the STAPL evaluation engine in a clean Java API, see [https://github.com/maartendecat/stapl-java-api][8].

Here is an example of using the Java API (taken from the [examples](https://github.com/maartendecat/stapl-examples/tree/master/src/main/scala/stapl/examples/javaapi)):

```java
package stapl.examples.javaapi;

import static stapl.javaapi.pdp.attributevalues.AttributeValueFactory.list;
import static stapl.javaapi.pdp.attributevalues.AttributeValueFactory.simple;
import stapl.core.AttributeContainer;
import stapl.core.Policy;
import stapl.core.Result;
import stapl.examples.policies.EdocsPolicy;
import stapl.javaapi.pdp.PDP;

public class SimpleExample {

	public static void main(String[] args) {
		/* Java imitation of the following Scala code:
		
		println(pdp.evaluate("subject1", "view", "resource1",
	        subject.role -> List("helpdesk"),
	        subject.tenant_name -> List("provider"),
	        subject.tenant_type -> List("provider"),
	        subject.assigned_tenants -> List("tenant1","tenant3"),
	        resource.type_ -> "document",
	        resource.owning_tenant -> "tenant4",
	        resource.confidential -> false))
		*/
		
		Policy policy = EdocsPolicy.policy();
		AttributeContainer subject = EdocsPolicy.subject();
		AttributeContainer resource = EdocsPolicy.resource();
		AttributeContainer action = EdocsPolicy.action();
		AttributeContainer env = EdocsPolicy.environment();
		
		PDP pdp = new PDP(policy);
		Result result = pdp.evaluate("subject1", "view", "resource1", 
				list(subject.get("role"), "helpdesk"),
				list(subject.get("tenant_name"), "provider"),
				list(subject.get("tenant_type"), "provider"),
				list(subject.get("assigned_tenants"), "tenant1", "tenant2"),
				simple(resource.get("type_"), "document"),
				simple(resource.get("owning_tenant"), "tenant4"),
				simple(resource.get("confidential"), false));
		System.out.println(result);
	}

}
```

For more examples, check the package `stapl.javaapi.examples`.

# Unit testing the policy

Using the interface to the policy engine shown above, STAPL policies can easily be tested for correctness in JUnit unit tests:

```scala
assert(pdp.evaluate("subject1", "view", "resource1",
        ... // any attributes for the test
    ) === Result(Permit, List(
        ... // any obligations
    )
))
```

For elaborate examples of policy testing, see the tests of [stapl-examples](https://github.com/maartendecat/stapl-examples).

# Logging

Wondering why a certain decision is Permit or Deny? STAPL can provide you with a detailed overview of the whole policy evaluation process. For example, for the policy above, this could be the logging output:

```
09:36:46.191 [main] DEBUG stapl.core.Policy - FLOW: starting evaluation of PolicySet #e-health example
09:36:46.194 [main] DEBUG stapl.core.pdp.BasicEvaluationCtx - FLOW: found value of SimpleAttribute(ACTION,id,String) in cache: view
09:36:46.194 [main] DEBUG stapl.core.pdp.BasicEvaluationCtx - FLOW: found value of SimpleAttribute(RESOURCE,type_,String) in cache: patient-data
09:36:46.194 [main] DEBUG stapl.core.pdp.BasicEvaluationCtx - FLOW: found value of ListAttribute(SUBJECT,roles,String) in cache: List(physician)
09:36:46.196 [main] DEBUG stapl.core.Rule - FLOW: starting evaluation of Policy #e-health example>requirement-for-permit
09:36:46.196 [main] DEBUG stapl.core.pdp.BasicEvaluationCtx - FLOW: found value of SimpleAttribute(RESOURCE,owner_id,String) in cache: patientX
09:36:46.196 [main] DEBUG stapl.core.pdp.BasicEvaluationCtx - FLOW: found value of ListAttribute(SUBJECT,treated,String) in cache: List(patientX)
09:36:46.196 [main] DEBUG stapl.core.Rule - FLOW: Policy #e-health example>requirement-for-permit returned Permit with obligations List()
09:36:46.198 [main] DEBUG stapl.core.Policy - FLOW: PolicySet #e-health example returned Result(Permit,List())
```

By default, logging is turned off. To turn it on, edit the file `stapl-core/src/main/resources/logback.xml` by changing the line 

```XML
<logger name="stapl.core" level="OFF"/>
```

to

```XML
<logger name="stapl.core" level="DEBUG"/>
```

# Getting started

Here are a few simple steps you can follow to get started with STAPL. If you want to skip even this explanation and want to dive right into STAPL, the [stapl-getting-started][9] project contains the resulting Maven project, just `git clone` it.

#### 1. Install Scala

STAPL was built using Scala 2.10. How to install Scala depends on your operating system. For Fedora:

```
> sudo yum install scala
```

#### 2. Get a good Scala editor

If you don't know where to look, try [http://scala-ide.org/][10].

#### 2. Get STAPL and install it locally in Maven

```
> git clone https://github.com/stapl-dsl/stapl-core
> cd stapl-core
> mvn install
```

#### 3. Create a new Scala Maven project

In the Scala IDE: Create a new Maven project with archetype `scala-archetype-simple` of group `net.alchim31.maven`. For more information: http://scala-ide.org/docs/tutorials/m2eclipse/

#### 4. Add the STAPL dependency

Add the following lines to the `<dependencies>` section in your `pom.xml`:

```XML
    <dependency>
    	<groupId>stapl</groupId>
    	<artifactId>stapl-core</artifactId>
    	<version>0.0.1-SNAPSHOT</version>
    </dependency>
```

#### 5. Create a new policy

The simplest way to create a new policy is to declare a Scala `object` and assign the policy to a `val`. This way, you can easily access the policy from other parts of your code, such as the policy decision point.

For example, add this simple policy to the end of the `App.scala` file generated by the Scala IDE:

```scala
import stapl.core._

object ExamplePolicy extends BasicPolicy {
  import stapl.core.dsl._
  
  subject.roles = ListAttribute(String)

  val policy = Policy("example policy") := when (action.id === "view") apply PermitOverrides to (
    Rule("permit physicians") := permit iff ("physician" in subject.roles),
    Rule("default deny") := deny
  )  
}
```

Explanation:

* The import `stapl.core._` imports the main part of STAPL, such as the `Policy` and `Rule` classes and the implicit conversions of strings and ints to STAPL attributes.
* The `BasicPolicy` class is a convenience class that avoids some boiler-plate code in your own policies. For example, by extending it, you automatically have `subject`, `resource`, `action` and `environment` declared in your scope. 
* `subject.roles = ListAttribute(String)` declares that subjects have an attribute called `Roles`, that is is multi-valued and that is is of the type `String`. After this declaration, you can use `subject.roles` in your policies.
* The policy itself is as explained above. As you can see, it's a simple policy that only applies to the action `view`, permits physicians using the `role` attribute and denies all other subjects.
* The assignment of the policy to a `val` is required to be able to access the policy from another part of your code, as shown in the example below.

#### 6. Use your new policy

Now that we have declared a new policy, you can use it by importing it, putting it in a policy decision point (PDP) and request an access decision.

For example, let's test the policy in the `main` method generated by the Scala IDE. First, add the following import to the top of `App.scala` (but below the `package` declaration):

```scala
import stapl.core.pdp._
```

Then change the main method of `object App` to the following:

```scala
def main(args : Array[String]) {
  import ExamplePolicy._
  val pdp = new PDP(policy)
  println(pdp.evaluate("subject1", "view", "resource1", 
		  subject.roles -> List("physician"))) // will return Permit
  println(pdp.evaluate("subject1", "view", "resource1", 
		  subject.roles -> List("another role"))) // will return Deny	  
  println(pdp.evaluate("subject1", "another action", "resource1", 
		  subject.roles -> List("physician"))) // will return NotApplicable
}
```

Explanation:

* The import imports everything we declared in `ExamplePolicy` into the current scope. Most importantly, this includes `subject,` `resource`, `action`, `env` and `policy`.
* The next line constructs a simple policy decision point with our example policy.
* The next lines request access decisions from the policy decision point, which will evaluate our example policy with the given attributes. The first evaluation will return Permit because of the `view` action and the role `physician`. The second evaluation will return Deny because of the wrong role. The third evaluation will return NotApplicable because of the incorrect action id. 

Some important notes here:

* Notice that you always need to provide the id of the subject, the id of the action and the id of the resource. This is needed because these ids are required for fetching other attributes of them should we incorporate and attribute database or something similar. After these ids, you can pass any number of attributes you want. 
* Notice that we did not include any `AttributeFinder`s. These can be used to allow the PDP to search for attributes you did not pass with the request at run-time, for example in a database. For more information, take a look [above][11].
* Notice that the PDP does not just return the decision, but also an empty list. This list contains the obligations which should be fulfilled with enforcing the access decision. It is empty because our example policy did not incorporate any obligations. For examples of obligations, check out the E-health policy in `stapl.core.examples`.

#### 7. Next

Try out some stuff yourself. If you want inspiration, take a look at the more elaborate examples in [stapl-examples](https://github.com/maartendecat/stapl-examples). Also take a look at the policy templates in  [stapl-templates](https://github.com/maartendecat/stapl-templates), they make it even easier for you to specify policies.



  [1]: #getting-started
  [2]: https://github.com/maartendecat/stapl-getting-started/
  [3]: #getting-started
  [4]: https://github.com/maartendecat/stapl-java-api
  [5]: #employing-the-engine-in-java
  [6]: https://github.com/PUMA-IAM/puma-sunxacml
  [7]: https://thrift.apache.org/
  [8]: https://github.com/maartendecat/stapl-java-api
  [9]: https://github.com/maartendecat/stapl-getting-started/
  [10]: http://scala-ide.org/
  [11]: #employing-the-engine-in-scala
