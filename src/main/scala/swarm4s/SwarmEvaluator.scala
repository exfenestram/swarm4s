package swarm4s

// Include the parallel collections
import scala.collection.parallel.CollectionConverters._



trait SwarmEvaluator {
    def evaluate(particles:Seq[PSO.Particle], func: PSO.Particle => PSO.Particle): Vector[PSO.Particle]
    def initialize(npart:Int, mins: Vector[Double], maxs: Vector[Double], f: Vector[Double] => Double): Vector[PSO.Particle]
}

class BasicSwarmEvaluator(minimize: Boolean) extends SwarmEvaluator {
    def evaluate(particles:Seq[PSO.Particle], func: PSO.Particle => PSO.Particle): Vector[PSO.Particle] = {
        val values = particles.map(func).toVector
        // Get the index of the particle in values with the highest best value
        val bestIndex = if (minimize) values.map { case (x, v, b, best) => best }.indexOf(values.map { case (x, v, b, best) => best }.min) else values.map { case (x, v, b, best) => best }.indexOf(values.map { case (x, v, b, best) => best }.max)
        values
    }

    def initialize(npart:Int, mins: Vector[Double], maxs: Vector[Double], f: Vector[Double] => Double): Vector[PSO.Particle] = {
        (0 until npart).map { _ => PSO.initParticle(mins, maxs, f) }.toVector
    }
}

class ParallelSwarmEvaluator(parallelism: Int, minimize:Boolean) extends SwarmEvaluator {
    val tasksupport = new scala.collection.parallel.ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelism))
    def evaluate(particles:Seq[PSO.Particle], func: PSO.Particle => PSO.Particle): Vector[PSO.Particle] = {
        val coll = particles.par
        coll.tasksupport = tasksupport
        val values = coll.map(func).toVector

        // Get the max or min index based on the best value
        val bestIndex = if (minimize) values.map { case (x, v, b, best) => best }.indexOf(values.map { case (x, v, b, best) => best }.min) else values.map { case (x, v, b, best) => best }.indexOf(values.map { case (x, v, b, best) => best }.max)
        values
    }

    def initialize(npart: Int, mins: Vector[Double], maxs: Vector[Double], f: Vector[Double] => Double): Vector[PSO.Particle] = {
        val coll = (0 until npart).par
        coll.tasksupport = tasksupport
        val ps = coll.map { _ => PSO.initParticle(mins, maxs, f) }.toVector
        val vs = ps.map { case (x, v, b, best) => best }
        val bestIndex = if (minimize) vs.indexOf(vs.min) else vs.indexOf(vs.max)
        ps
    }
}