object Map{

	type coord = Int
	case class Position(x: coord, y: coord){
		override def toString() = s"($x, $y)"
	}

	abstract class gameMap(val size: Int){
		def makePosition(x: coord, y: coord) = Position((x + size) % size, (y + size) % size)
		def distance(us: Position, them: Position) = math.sqrt((us.x - them.x)*(us.x - them.x) + (us.y - them.y)*(us.y - them.y))
	}

	class Movable(var position: Position, val moveSpeed: Int = 1) extends gameMap(16){
		def right():Unit = position = makePosition(position.x + moveSpeed, position.y)
		def left(): Unit = position = makePosition(position.x - moveSpeed, position.y)
		def up(): 	Unit = position = makePosition(position.x, position.y + moveSpeed)
		def down(): Unit = position = makePosition(position.x, position.y - moveSpeed)
	}

	trait Speed extends Movable{ override val moveSpeed = 2}


}