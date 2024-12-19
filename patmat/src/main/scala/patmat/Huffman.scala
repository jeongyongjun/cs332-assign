package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree



  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(_, _, _, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, _) => List(c)
    case Fork(_, _, cs, _) => cs
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def updateCount(map: Map[Char, Int], char: Char): Map[Char, Int] =
      map + (char -> (map.getOrElse(char, 0) + 1))
    chars.foldLeft(Map.empty[Char, Int])(updateCount).toList
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortWith(_._2 < _._2).map { case (c, w) => Leaf(c, w) }
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case _ :: Nil => true
    case _ => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case first :: second :: rest =>
      val combined = makeCodeTree(first, second)
      (combined :: rest).sortWith((a, b) => weight(a) < weight(b))
    case _ => trees
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(pred: List[CodeTree] => Boolean, op: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree =
    if (pred(trees)) trees.head else until(pred, op)(op(trees))

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars)))



  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def traverse(subtree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = subtree match {
      case Leaf(c, _) => (c, bits)
      case Fork(left, right, _, _) =>
        if (bits.head == 0) traverse(left, bits.tail) else traverse(right, bits.tail)
    }

    def decodeAll(bits: List[Bit], acc: List[Char]): List[Char] =
      if (bits.isEmpty) acc
      else {
        val (char, remainingBits) = traverse(tree, bits)
        decodeAll(remainingBits, acc :+ char)
      }

    decodeAll(bits, List.empty)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(
    Fork(
      Fork(Leaf('s',121895),Fork(Leaf('d',56269),Leaf('x',5928),List('d','x'),62197),List('s','d','x'),184092),
      Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),
      List('s','d','x','o','l'),350522),
    Fork(
      Fork(Leaf('r',100500),Fork(Leaf('c',50003),Leaf('n',108812),List('c','n'),158815),List('r','c','n'),259315),
      Leaf('e',225947),
      List('r','c','n','e'),485262),
    List('s','d','x','o','l','r','c','n','e'),835784)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeChar(subtree: CodeTree, char: Char, acc: List[Bit]): List[Bit] = subtree match {
      case Leaf(c, _) if c == char => acc
      case Fork(left, right, _, _) =>
        if (chars(left).contains(char)) encodeChar(left, char, acc :+ 0)
        else encodeChar(right, char, acc :+ 1)
    }
    text.flatMap(char => encodeChar(tree, char, List()))
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    table.find(_._1 == char).map(_._2).getOrElse(throw new Error("Character not found in code table"))

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(c, _) => List((c, List()))
    case Fork(left, right, _, _) =>
      mergeCodeTables(
        convert(left).map { case (c, bits) => (c, 0 :: bits) },
        convert(right).map { case (c, bits) => (c, 1 :: bits) }
      )
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    text.flatMap(char => codeBits(table)(char))
  }
}

