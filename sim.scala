import Fighters._
import scala.util.Random
object sim extends App{

	def decideAction(f: Fighter, enemies: List[Fighter]){
			Random.nextInt(15) % 4 match {
			case 0 => f.right
			case 1 => f.left
			case 2 => f.up
			case 3 => f.down
		}

		f.strike(enemies(Random.nextInt(enemies.length)))
	}

	def turn(f: Fighter, enemies: List[Fighter]) = decideAction(f, enemies)

	def doneFighting(heros: List[Fighter]): Boolean = {
		if(heros.map(h => if(h.alive) 1 else 0).sum <= 1) return true
		else return false
	}

	val heros = List(new Warrior(), new Mage(), new Archer())

	def simFight(heros: List[Fighter], i: Int = 0): Unit = {
		if (doneFighting(heros)) println(s"$heros is the WINNER")
		else{ 
			val j = i % heros.length
			turn(heros(j), heros.take(j) ++ heros.drop(j + 1))
			simFight(heros.filter(_.alive), i + 1)
		}
	}

	simFight(heros)
}
