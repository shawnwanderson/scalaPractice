object Map{

	class Position(posX: Int, posY: Int){
		val x = (posX + 16) % 16
		val y = (posY + 16) % 16
		def distance(that: Position) = math.sqrt((this.x - that.x)*(this.x - that.x) + (this.y - that.y)*(this.y - that.y))
		override def toString() = s"($x, $y)"
	}

	abstract class Movable(var position: Position, val moveSpeed: Int = 1){
		def right():Unit = position = new Position(position.x + moveSpeed, position.y)
		def left(): Unit = position = new Position(position.x - moveSpeed, position.y)
		def up(): 	Unit = position = new Position(position.x, position.y + moveSpeed)
		def down(): Unit = position = new Position(position.x, position.y - moveSpeed)
		def distance(that: Movable) = this.position.distance(that.position)
	}


}