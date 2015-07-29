object Fighters{	
	import scala.util.Random
	import Map._
	import Equip._
	import Caster._
	abstract class Fighter(h: Int, d: Int, r: Int, p: Position = new Position(Random.nextInt(15), Random.nextInt(15))) extends Movable(p){
		var health: Int = h
		val damage: Int = d
		val range: Int = r
		def strike(enemy: Fighter): Unit = {
			if (inRange(enemy.position)) {
				hit(enemy)
				println(s"$this hit $enemy for ${this.damage} damage")
			}
		}
		def hit(enemy: Fighter): Unit = enemy.getHit(damage)
		def getHit(dmg: Int): Unit = {
			health = health - dmg
			if (health <= 0) println(s"$this HAS DIED")
		}
		def inRange(enemy: Position): Boolean = distance(position, enemy) <= range
		def alive(): Boolean = health > 0
		def heal(h: Int): Unit = health = health + h
	}

	class Archer(h: Int = 35, d: Int = 4, r: Int = 4) extends Fighter(h, d, r) with Speed{
		override def toString() = s"Archer at $position with $health health"
	}

	class Warrior(h: Int = 45, d: Int = 7, r: Int = 2) extends Fighter(h, d, r) with Sheild with Helm{
		override def toString() = s"Warrior at $position with $health health"
	}

	class Mage(h: Int = 35, d: Int = 3, r: Int = 2, m: Int = 50, spells: List[Magic] = List(new Fira, new Cura)) extends Caster(h, d, r, m, spells){
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


}