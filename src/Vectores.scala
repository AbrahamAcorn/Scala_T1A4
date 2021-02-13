object Vectores {
  def fMedia(vector:Array [Int]): Double={
    var suma=0.0
    for(dat<-vector)
      suma+=dat.toDouble
    return suma/vector.length
  }
  def fModa(vector:Array [Int]): Double={
    var maxRep= 0
    var moda= 0

    for(dat1<-vector){
      var numRep= 0;
      for(dat2<-vector){
        if(dat1==dat2){
          numRep+=1
        }
        if(numRep>maxRep){
          moda= dat1;
          maxRep= numRep;
        }
      }
    }
    return moda
  }
  def fMediana(vector:Array [Int]): Double={
    var mediana=0.0
    scala.util.Sorting.quickSort(vector)
    if(vector.length % 2 == 0){
      var sumaMedios = vector(vector.length / 2) + vector(vector.length /2- 1)
      mediana = sumaMedios/2
    } else {
      mediana = vector(vector.length/2)
    }
    return mediana
  }
  def fDesviacion(vector:Array [Int], media:Double): Unit={
    var desv=0.0
    for(dat<-vector){
      desv=dat.toDouble-media
      println("Para " + dat + " es de: "+desv.abs)
    }

  }
  def fDesvMedia(vector:Array [Int], media:Double): Double={
    var desvM=0.0
    var sum=0.0
    for(dat<-vector){
      desvM=(dat.toDouble-media)
      sum+=desvM
    }
    return sum.abs/vector.length
  }
  def fVarianza(vector:Array [Int], media:Double): Double={
    var desvM=0.0
    var sum=0.0
    for(dat<-vector){
      desvM=(dat.toDouble-media)
      sum+=Math.pow(desvM,2)
    }
    return sum.abs/vector.length
  }
  def fDesvEstandar(varianza: Double): Double={
    return Math.sqrt(varianza)
  }

  def main(args: Array[String]): Unit = {
    var j=0
    println("Ingresa el tamaño del vector:")
    j=scala.io.StdIn.readInt()


    var edad:Array[Int] = new Array[Int](j)
    println("tamño_ "+edad.length)
    for(i<- 0 until j) {
      println("Ingresa  el valor "+i+":")
      edad(i)=scala.io.StdIn.readInt()
    }

    //var edad=Array(23,45,23,28,55,23,21,12,33,33)

    val med=fMedia(edad)
    println("La media es: "+med)
    println("la Moda es: %f".format(fModa(edad)))
    println("La mediana es "+ fMediana(edad))
    println("La desviacion respecto a la media es de:")
    fDesviacion(edad,med)
    println("La desviacion media es de: "+ fDesvMedia(edad, med))
    val varian=fVarianza(edad,med)
    println("La varianza es de: ")
    println("La desviacion estandar es: "+fDesvEstandar(varian))

  }

}
