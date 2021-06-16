trait Queue {
  var queue: List[Double] = List.empty
  var front_element: Int = -1
  var rear_element: Int = -1

  def enqueue(item: Double): String = {
    if (rear_element == -1 && front_element == -1) {
      front_element = front_element + 1
      rear_element = rear_element + 1
      queue = queue ::: List(item)
      "Element is enqueued"
    }
    else {
      rear_element = rear_element + 1
      queue = queue ::: List(item)
      "Element is enqueued"
    }
  }

  def dequeue(item: Double): String = {
    if (front_element == -1 && rear_element == -1) {
      "Underflow."
    }
    else if (front_element == rear_element) {
      queue = queue.drop(1)
      front_element = -1
      rear_element = -1
      "Element dequeued."
    }
    else {
      queue = queue.drop(1)
      front_element = front_element + 1
      "Element dequeued."
    }
  }
  def getQueue: List[Double] = {
    queue
  }
}

class DoubleQueue extends Queue
{
  override def enqueue(item: Double): String =
  {
    if(rear_element == -1 && front_element == -1)
    {
      front_element = front_element + 1
      rear_element = rear_element + 1
      val ItemDouble = 2 * item
      queue = queue ::: List(ItemDouble)
      "Enqueued"
    }
    else
    {
      rear_element = rear_element + 1
      val ItemDouble = 2 * item
      queue = queue ::: List(ItemDouble)
      "Enqueued"
    }
  }
}
class SquareQueue extends Queue
{
  override def enqueue(item: Double): String =
  {
    if(rear_element == -1 && front_element == -1)
    {
      front_element = front_element + 1
      rear_element = rear_element + 1
      val ItemSquare = item * item
      queue = queue ::: List(ItemSquare)
      "Enqueued"
    }
    else
    {
      rear_element = rear_element + 1
      val ItemSquare = item * item
      queue = queue ::: List(ItemSquare)
      "Enqueued"
    }
  }
}
object QueueObject extends App
{
  val double_queue_obj = new DoubleQueue
  val square_queue_obj = new SquareQueue
  println(double_queue_obj.enqueue(10))
  println(double_queue_obj.enqueue(12))
  println(double_queue_obj.enqueue(30))
  println(double_queue_obj.enqueue(40))
  println(double_queue_obj.getQueue)
  println(square_queue_obj.enqueue(50))
  println(square_queue_obj.enqueue(60))
  println(square_queue_obj.enqueue(70))
  println(square_queue_obj.enqueue(80))
  println(square_queue_obj.getQueue)
  println(square_queue_obj.dequeue(item = 40))
  println(square_queue_obj.getQueue)
  println(square_queue_obj.dequeue(item = 40))
  println(square_queue_obj.getQueue)
}