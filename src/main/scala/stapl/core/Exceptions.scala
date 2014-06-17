package stapl.core

class TypeCheckException(message: String) extends RuntimeException(message) {
  
  def this(found: AttributeType, expected: AttributeType) = this(s"Found AttributeType '$found', but expected AttributeType '$expected'.")
}

class AttributeNotFoundException(attribute: Attribute) extends RuntimeException(s"$attribute wasn't found.")

class AttributeDoesNotExistException(name: String) extends RuntimeException(s"No attribute with name '$name' has been defined.")