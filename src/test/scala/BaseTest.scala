import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

abstract class BaseTest extends AnyFlatSpec with PrivateMethodTester
