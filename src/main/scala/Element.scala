trait Element {
  def id: String
  def positiveNode: Int
  def negativeNode: Int
  def isGrounded: Boolean = positiveNode == 0 || negativeNode == 0
}
