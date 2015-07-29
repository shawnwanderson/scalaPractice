object Caster {
	import Fighters._
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
}