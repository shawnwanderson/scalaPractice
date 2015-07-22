import scala.util.Random

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


abstract class Fighter(h: Int, d: Int, r: Int, p: Position = new Position(Random.nextInt(15), Random.nextInt(15))) extends Movable(p){
	var health: Int = h
	val damage: Int = d
	val range: Int = r
	def strike(enemy: Fighter): Boolean = {
		if (inRange(enemy)) {
			hit(enemy)
			return true
		}
		else return false
	}
	def hit(enemy: Fighter): Unit = enemy.getHit(damage)
	def getHit(dmg: Int): Unit = health = health - dmg
	def inRange(enemy: Fighter): Boolean = distance(enemy) <= range
	def alive(): Boolean = health > 0
}

trait speed extends Movable{ override val moveSpeed = 2}

trait sheild extends Fighter{ 
	override def getHit(dmg: Int): Unit = {
		if(Random.nextInt(100) > 35){
			health = health - dmg
			} 
			else println("Attack was blocked!")
		}
	}

class Archer(h: Int = 50, d: Int = 4, r: Int = 4) extends Fighter(h, d, r) with speed{
	override def toString() = s"Archer at $position with $health health"
}

class Warrior(h: Int = 75, d: Int = 7, r: Int = 2) extends Fighter(h, d, r) with sheild{
	override def toString() = s"Warrior at $position with $health health"
}

def turn(f: Fighter, enemy: Fighter) = {

	println(s"$f")
	Random.nextInt(15) % 4 match {
		case 0 => f.right
		case 1 => f.left
		case 2 => f.up
		case 3 => f.down
	}
	if(f.strike(enemy)){
		println(s"$f hit $enemy for ${f.damage} damage")
	}
}

def simFight() = {
	val a = new Archer
	val w = new Warrior

	var i = 0
	while(a.alive && w.alive){
		i % 2 match {
			case 0 => turn(a, w)
			case 1 => turn(w, a)
		}
		i = i + 1
	}
	if(a.alive) println("Archer wins!") else println("Warrior wins!")
}
