import util.{Pixel, Util}

import java.awt.Image
import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    def split(delim: Char)(str: List[Char]): List[List[Char]] = {
      def op(c: Char, acc: List[List[Char]]): List[List[Char]] = {
        acc match
          case Nil => if (c == delim) Nil else List(List(c))
          case x :: xs => if (c == delim) Nil :: acc else (c :: x) ::xs
      }
      str.foldRight(Nil)(op)
    }

    /**
     *  Converts List[Char] to Int
     * @param str number string
     * @return Int number
     */
    def listToInt(str: List[Char]): Int = str.foldLeft(0)((acc, c) => acc * 10 + c.asDigit)

    /**
     * Converts list of strings to corresponding Pixel
     * @param line pixel line from .ppm file
     * @return Pixel object
     */
    def lineToPixel(line: List[List[Char]]): Pixel = {
      line.map(listToInt) match
        case r :: g :: b :: Nil => Pixel(r, g, b)
        case _ => Pixel(0, 0, 0)
    }

    val lines = split('\n')(image)
      .drop(1)
      .map(split(' '))

    val pixels = lines.drop(2).map(lineToPixel)
    val perLine = lines.head.map(listToInt).head

    pixels.grouped(perLine).toList
  }

  def toStringPPM(image: Image): List[Char] = {
    /**
     * Converts a pixel to corresponding string
     * for .ppm file
     * @param p pixel
     * @return .ppm file pixel string
     */
    def pixelToLine(p: Pixel): List[Char] = {
      List(
        p.red.toString.toList,
        ' ' :: p.green.toString.toList,
        ' ' :: p.blue.toString.toList
      ).flatten ++ List('\n')
    }

    /**
     * Calculates the nr. of lines and columns for
     * a matrix image
     * @param image matrix image
     * @return tuple with image dimensions (length, height)
     */
    def getDimensions(image: Image): (Int, Int) = (image.head.size ,image.size)

    val (length, height) = getDimensions(image)
    val pixelsList = image.flatten
      .flatMap(pixelToLine)

    "P3\n".toList ++
      length.toString.toList ++ List(' ') ++ height.toString.toList ++ List('\n') ++
    "255\n".toList ++
      pixelsList
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = image1 ++ image2

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = (image1.transpose ++ image2.transpose).transpose

  // ex 3
  @tailrec
  def rotate(image: Image, degrees: Integer): Image = {
    def rotate90(image: Image): Image = image.map(_.reverse).transpose

    degrees match
      case 360 => image
      case 90 => rotate90(image)
      case _ => rotate(rotate90(image), degrees - 90)
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  /**
   * Applies an operator on 2 matrices with the
   * same dimensions
   * @param op operator to be applied between
   *           image elements
   * @return result matrix after scalar operator
   */
  def matScalarOperator(m1: GrayscaleImage, m2: GrayscaleImage)(op: (Double, Double) => Double): GrayscaleImage = {
    m1.zip(m2)
      .map(line =>
        line._1.zip(line._2).map(op(_, _))
      )
  }

  def edgeDetection(image: Image, threshold : Double): Image = {
    /**
     * Converts grayscale pixel to RGB pixel
     * @param value grayscale pixel
     * @return white/black pixel according to threshold
     */
    def convertToPixel(value: Double): Pixel =
      if (value < threshold) Pixel(0, 0, 0)
      else Pixel(255, 255, 255)

    val grayscale = image.map(_.map(Util.toGrayScale))

    val blurred = applyConvolution(grayscale, gaussianBlurKernel)
    val Mx = applyConvolution(blurred, Gx).map(_.map(_.abs))
    val My = applyConvolution(blurred, Gy).map(_.map(_.abs))

    matScalarOperator(Mx, My)(_+_).map(_.map(convertToPixel))
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
    def convolutionOperator(section: GrayscaleImage): Double =
      matScalarOperator(section, kernel)(_*_).flatten.sum

    Util.getNeighbors(image, kernel.size / 2)
      .map(_.map(convolutionOperator))
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    /**
     * Calculates a row of the pascal triangle
     * based on the previous row
     * @return a new pascal triangle row
     */
    def makePascalRow(previousRow: List[Int]): List[Int] = {
      def calcPosition(position: Int): Int =
        (previousRow(position - 1) + previousRow(position)) % m

      @tailrec
      def aux(newRow: List[Int]): List[Int] = {
        val i = newRow.size
        if (i < previousRow.size) aux(newRow ++ List(calcPosition(i)))
        else newRow ++ List(1)
      }

      aux(List(1))
    }

    /**
     * Add black pixels to fill a row for
     * the pascal triangle image
     */
    @tailrec
    def addPadding(row: List[Pixel]): List[Pixel] = {
      if (row.size < size) addPadding(row ++ List(Pixel(0, 0, 0)))
      else row
    }

    /**
     * Creates a pascal triangle
     */
    def makePascal(): List[List[Int]] = {
      @tailrec
      def aux(matrix: List[List[Int]]): List[List[Int]] = {
        if (matrix.size < size) aux(makePascalRow(matrix.head) :: matrix)
        else matrix.reverse
      }

      aux(List(List(1)))
    }

    makePascal().map(_.map(pix => funct(pix)))
      .map(addPadding)
  }
}
