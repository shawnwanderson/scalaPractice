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
	def strike(enemy: Fighter): Unit = {
		if (inRange(enemy)) {
			hit(enemy)
			println(s"$this hit $enemy for ${this.damage} damage")
		}
	}
	def hit(enemy: Fighter): Unit = enemy.getHit(damage)
	def getHit(dmg: Int): Unit = health = health - dmg
	def inRange(enemy: Fighter): Boolean = distance(enemy) <= range
	def alive(): Boolean = health > 0
	def heal(h: Int): Unit = health = health + h
}

abstract class Magic(var mana: Int){
	def cast(enemy: Option[Fighter])
}

abstract class Caster(h: Int, d: Int, r: Int, m: Int, magic: List[Magic]) extends Fighter(h, d, r){
	var mana = m
	def cast(spell: Magic, enemy: Option[Fighter]) = {
		spell.cast(enemy)
		mana = mana - spell.mana
	}
}

class Fira extends Magic(5){
	def cast(enemy: Option[Fighter]): Unit = enemy match {
		case Some(e) => 
			e.getHit(10)
			println(s"$e hit with Fira for 10 damage")
		case None 	 => println("no target for Fira")
	}
}

class Cura extends Magic(7){
	def cast(target: Option[Fighter]): Unit = target match {
		case Some(t) => 
			t.heal(15)
			println(s"$t is healed for 15 health")
		case None 	 => println("No target for heal") 
	}
}


trait Armour extends Fighter{
	val defense: Int
	override def getHit(dmg: Int): Unit = {
		health = health + defense
		super.getHit(dmg)
	}
}

trait Helm extends Armour{override val defense = 1}

trait Speed extends Movable{ override val moveSpeed = 2}

trait Sheild extends Fighter{ 
	override def getHit(dmg: Int): Unit = {
		if(Random.nextInt(100) > 35){
			health = health - dmg
			} 
			else println("Attack was blocked!")
		}
	}

class Mage(h: Int = 35, d: Int = 2, r: Int = 2, m: Int = 25, spells: List[Magic] = List(new Fira, new Cura)) extends Caster(h, d, r, m, spells){
	override def toString() = s"Mage at $position with $health health and $mana mana"
	override def strike(enemy: Fighter): Unit = {
		val currentSpell = spells(Random.nextInt(spells.length))
		if (mana > currentSpell.mana){ currentSpell match {
			case s: Cura => cast(currentSpell, Some(this))
			case s: Fira => cast(currentSpell, Some(enemy))
		}
		}
		else super.strike(enemy)
	}
}


class Archer(h: Int = 35, d: Int = 4, r: Int = 4) extends Fighter(h, d, r) with Speed{
	override def toString() = s"Archer at $position with $health health"
}

class Warrior(h: Int = 45, d: Int = 7, r: Int = 2) extends Fighter(h, d, r) with Sheild with Helm{
	override def toString() = s"Warrior at $position with $health health"
}

def turn(f: Fighter, enemies: List[Fighter]) = {

	//println(s"$f")
	Random.nextInt(15) % 4 match {
		case 0 => f.right
		case 1 => f.left
		case 2 => f.up
		case 3 => f.down
	}

	f.strike(enemies(Random.nextInt(enemies.length)))
}

def simFight() = {
	val a = new Archer
	val w = new Warrior
	val m = new Mage

	var i = 0
	while(a.alive && w.alive && m.alive){
		i % 3 match {
			case 0 => turn(a, List(w, m))
			case 1 => turn(w, List(a, m))
			case 2 => turn(m, List(a, w))
		}
		i = i + 1
	}
	if(a.alive) println("Archer wins!") else println("Warrior wins!")
}
