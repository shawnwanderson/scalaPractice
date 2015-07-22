import scala.util.Random

class Position(posX: Int, posY: Int){
	val x = (posX + 16) % 16
	val y = (posY + 16) % 16
	def distance(that: Position) = math.sqrt((this.x - that.x)*(this.x - that.x) + (this.y - that.y)*(this.y - that.y))
	override def toString() = s"($x, $y)"
}

abstract class Movable(var position: Position){
	def right():Unit = position = new Position(position.x + 1, position.y)
	def left(): Unit = position = new Position(position.x - 1, position.y)
	def up(): 	Unit = position = new Position(position.x, position.y + 1)
	def down(): Unit = position = new Position(position.x, position.y - 1)
	def distance(that: Movable) = this.position.distance(that.position)
}

abstract class Fighter(p: Position, h: Int, d: Int, r: Int) extends Movable(p){
	var health: Int = h
	val damage: Int = d
	val range: Int = r
	def strike(enemy: Fighter): Unit = if (inRange(enemy)) hit(enemy)
	def hit(enemy: Fighter): Unit = enemy.getHit(damage)
	def getHit(dmg: Int): Unit = health = health - dmg
	def inRange(enemy: Fighter): Boolean = distance(enemy) <= range
}

class Archer(p: Position, h: Int = 50, d: Int = 4, r: Int = 4) extends Fighter(p, h, d, r){
	override def toString() = s"Archer at $position"
}

class Warrior(p: Position, h: Int = 75, d: Int = 7, r: Int = 1) extends Fighter(p, h, d, r){
	override def toString() = s"Warrior at $position"
}

def turn(f: Fighter, enemy: Fighter) = {
	Random.nextInt % 4 match {
		case 0 => f.right
		case 1 => f.left
		case 2 => f.up
		case 3 => f.down
	}
	f.strike(enemy)
}

def simFight() = {
	val a = new Archer(new Position(Random.nextInt, Random.nextInt))
	val w = new Warrior(new Position(Random.nextInt, Random.nextInt))

	while(true){


	}
}
