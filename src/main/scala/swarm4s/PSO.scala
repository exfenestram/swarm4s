package swarm4s

// include scala parallel collections
import scala.collection.parallel.CollectionConverters._

class PSO(eval: SwarmEvaluator, mins: Vector[Double], maxs: Vector[Double], w: Double, c1: Double, c2:Double, npart: Int, minimize:Boolean, func: Vector[Double] => Double) {

    require(mins.length == maxs.length, "mins and maxs must have the same length")

    var particles = eval.initialize(npart, mins, maxs, func)
    // Get the index of the particle in particles with the lowest best value
    var leader = extract_leader

    def extract_leader : Int = {
        val values = particles.map { case (x, v, b, best) => best }
        if (values.length > 0) {
            if (minimize) values.indexOf(values.min) else values.indexOf(values.max)
        } else {
            throw new IllegalArgumentException("The particles list is empty")   
        }
    }

    // make_bounds checks the vector that each element lies between maxs and mins
    // If the bound is outside that range, it is set to a random position between the min and max
    def make_bounds(x: Vector[Double], mins: Vector[Double], maxs: Vector[Double]) : Vector[Double] = {
       x.zip(mins).zip(maxs).map { case ((xi, min), max) => if (xi < min || xi > max) min + (max - min) * scala.util.Random.nextDouble() else xi }
    }

    def comp(v1: Double, v2: Double) : Boolean = {
        if (minimize) v1 < v2 else v1 > v2
    }

    def randparticle : PSO.Particle = {
        val rint = scala.util.Random.nextInt(particles.length)
        particles(rint)
    }

    def randbest : Vector[Double] = {
        val (x, v, b, best) = randparticle
        b
    }

    def randdouble : Double = {
        scala.util.Random.nextDouble()
    }

    def update(p: PSO.Particle) : PSO.Particle = {
        val (x, v, b, best) = p
        val newV = v.zip(x).zip(b).zip(randbest).map { case (((vi, xi), bi), gxi) =>
        
            w * vi + c1 * randdouble * (bi - xi) + c2 * randdouble * (gxi - xi)
        }
        val newX_tmp = x.zip(newV).map { case (xi, vi) => xi + vi }
        val newX = make_bounds(newX_tmp, mins, maxs)    
        val newF = PSO.evaluate((newX, newV, b, best), func)
        if (comp(newF, best)) (newX, newV, newX, newF) else (newX, newV, b, best)
    } 

    def iteration : PSO.Particle = {

        def do_update(p: PSO.Particle) : PSO.Particle = {
            update(p)
        }

        particles = eval.evaluate(particles, do_update)
        leader = extract_leader
        particles(leader)
    }
}


object PSO {

    // Particle is a tuple of position, velocity, best position and best value
    type Particle = (Vector[Double], Vector[Double], Vector[Double], Double)


    def initParticle(mins: Vector[Double], maxs:Vector[Double], f: Vector[Double]=>Double) : Particle = {
        require(mins.length == maxs.length, "mins and maxs must have the same length")
        val x = mins.zip(maxs).map { case (min, max) => min + (max - min) * scala.util.Random.nextDouble() }
        val v = mins.zip(maxs).map { case (min, max) => (max - min) * (scala.util.Random.nextDouble() - 0.5) }
        (x, v, x, f(x))
    }

    def evaluate(p: Particle, f: Vector[Double] => Double) : Double = {
        f(p._1)
    }

    def main_idea(args: Array[String]) : Unit = {

        def op(x: Vector[Double]) : Double = {
            Math.abs(x(0) + x(1)) + Math.abs(x(0) * x(1))
        }
        val pso = new PSO(new BasicSwarmEvaluator(true), Vector(-1.0, -1.0), Vector(1.0, 1.0), 0.75, 1.6, 1.4, 10, true, op)
        for (i <- 0 until 1000) {
            val vec = pso.iteration
            println(s"Best: ${vec._3} ${vec._4}")
        }
    }


}