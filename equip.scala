object Equip{
	import Fighters._
	import scala.util.Random

	trait Armour extends Fighter{
		val defense: Int
		override def getHit(dmg: Int): Unit = {
			health = health + defense
			super.getHit(dmg)
		}
	}
	trait Helm extends Armour{override val defense = 1}
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

}