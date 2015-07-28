object Fighters{	
	import scala.util.Random
	import Map._
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
		def getHit(dmg: Int): Unit = {
			health = health - dmg
			if (health <= 0) println(s"$this HAS DIED")
		}
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
				else {
					println("Attack was blocked!")
					super.getHit(dmg / 2)
				}
			}
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


	class Archer(h: Int = 35, d: Int = 4, r: Int = 4) extends Fighter(h, d, r) with Speed{
		override def toString() = s"Archer at $position with $health health"
	}

	class Warrior(h: Int = 45, d: Int = 7, r: Int = 2) extends Fighter(h, d, r) with Sheild with Helm{
		override def toString() = s"Warrior at $position with $health health"
	}
}