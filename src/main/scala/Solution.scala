import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board
  val puzzle = "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
  val puzzle2 = "....8............................................................................"
  val lst1 = List(1,2,3,4,5,6,7,8,9)
  //val emptyBoard = Map(0.to(8).map{r=> 0.to(8).map{c => (r,c)}} -> List(1,2,3,4,5,6,7,8,9))
  val emptyBoard = Map((0,0) -> lst1, (0,1) -> lst1, (0,2) -> lst1, (0,3) -> lst1, (0,4) -> lst1, (0,5) -> lst1, (0,6) -> lst1, (0,7) -> lst1, (0,8) -> lst1, 
  						(1,0) -> lst1, (1,1) -> lst1, (1,2) -> lst1, (1,3) -> lst1, (1,4) -> lst1, (1,5) -> lst1, (1,6) -> lst1, (1,7) -> lst1, (1,8) -> lst1, 
  						(2,0) -> lst1, (2,1) -> lst1, (2,2) -> lst1, (2,3) -> lst1, (2,4) -> lst1, (2,5) -> lst1, (2,6) -> lst1, (2,7) -> lst1, (2,8) -> lst1, 
  						(3,0) -> lst1, (3,1) -> lst1, (3,2) -> lst1, (3,3) -> lst1, (3,4) -> lst1, (3,5) -> lst1, (3,6) -> lst1, (3,7) -> lst1, (3,8) -> lst1, 
  						(4,0) -> lst1, (4,1) -> lst1, (4,2) -> lst1, (4,3) -> lst1, (4,4) -> lst1, (4,5) -> lst1, (4,6) -> lst1, (4,7) -> lst1, (4,8) -> lst1, 
  						(5,0) -> lst1, (5,1) -> lst1, (5,2) -> lst1, (5,3) -> lst1, (5,4) -> lst1, (5,5) -> lst1, (5,6) -> lst1, (5,7) -> lst1, (5,8) -> lst1, 
  						(6,0) -> lst1, (6,1) -> lst1, (6,2) -> lst1, (6,3) -> lst1, (6,4) -> lst1, (6,5) -> lst1, (6,6) -> lst1, (6,7) -> lst1, (6,8) -> lst1, 
  						(7,0) -> lst1, (7,1) -> lst1, (7,2) -> lst1, (7,3) -> lst1, (7,4) -> lst1, (7,5) -> lst1, (7,6) -> lst1, (7,7) -> lst1, (7,8) -> lst1, 
  						(8,0) -> lst1, (8,1) -> lst1, (8,2) -> lst1, (8,3) -> lst1, (8,4) -> lst1, (8,5) -> lst1, (8,6) -> lst1, (8,7) -> lst1, (8,8) -> lst1)



  def parse(str: String): Board = {
	new Board(parseHelper(str, emptyBoard, 0))
  }

  def parseHelper(str: String, board: Map[(Int, Int), List[(Int)]], index: Int): Map[(Int, Int), List[(Int)]] = {
  	val r = index/9
  	val c = index%9
  	

  	if(str(index)=='.') {
  		if(index==80){board}
  		else{parseHelper(str, board, index+1)}
  	}
  	else {//board((r,c)).filter(x => x==num)
  		val num = str(index).asDigit
  		if(index==80){parseHelper2(num, board + ((r,c) -> List(num)), peers(r,c))}
  		else{parseHelper(str, parseHelper2(num, board + ((r,c) -> List(num)), peers(r,c)), index +1)}
  	}
  }

  def parseHelper2(num: Int, board: Map[(Int, Int), List[(Int)]], lst: List[(Int, Int)]): Map[(Int, Int), List[(Int)]] = lst match {
  	case h :: t => parseHelper2(num, board + (h -> board(h).filterNot(x => x==num)), t)
  	case Nil => board
  }
 
  // You can use a Set instead of a List (or, any Iterable)
  def peers (row: Int , col: Int): List[(Int , Int)] = {
	val peersTbl = Map((0.to(8).flatMap{r =>
		0.to(8).map{c =>
			((r,c) -> calcPeers(r,c))
		}
	}):_*)
	peersTbl((row,col))
}
  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
	val rowPeers = 0.to(8).map{((_, col))} // r=>(r, col)
	val colPeers = 0.to(8).map{((row, _))} // c=>(row, c)
	val boxRow: Int = (row/3)*3
	val boxCol: Int = (col/3)*3
	val boxPeers = boxRow.to(boxRow+2).flatMap{r=> boxCol.to(boxCol+2).map{c=>(r,c)}}

	(rowPeers ++ colPeers ++ boxPeers).filterNot{case (r,c) => (r,c)==(row,col)}.toList.distinct
}
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    //throw new UnsupportedOperationException("not implemented")
    if(!available((row,col)).isEmpty)
    	Option(available((row,col)).head)
    else
    	None
  }

  def isSolved(): Boolean = {
    //throw new UnsupportedOperationException("not implemented")
    available.values.toList.forall(x => x.length==1)
  }

  def isUnsolvable(): Boolean = {
    //throw new UnsupportedOperationException("not implemented")
    available.values.toList.forall(x => x.isEmpty)
    // val asdf = available.values.toList.flatten
    // val lst0 = asdf.slice(0,8)
    // val lst1 = asdf.slice(9,17)
    // val lst2 = asdf.slice(18,26)
    // val lst3 = asdf.slice(27,35)
    // val lst4 = asdf.slice(36,44)
    // val lst5 = asdf.slice(45,53)
    // val lst6 = asdf.slice(54,62)
    // val lst7 = asdf.slice(63,71)
    // val lst8 = asdf.slice(72,80)
    // val bigLst = List(lst0, lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8)
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    new Board(Solution.parseHelper2(value, available + ((row,col) -> available((row,col)).filterNot(x => x==value)), Solution.peers(row,col)))
    //new Board(available + ((row,col) -> available.availableValuesAt(row,col).filterNot(value)))
    //throw new UnsupportedOperationException("not implemented")
  }

  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if (isUnsolvable()) {
      List()
    }
    else


    throw new UnsupportedOperationException("not implemented")
  }

  def solve(): Option[Board] = {
    throw new UnsupportedOperationException("not implemented")
  }

  // def parseHelper2(num: Int, board: Map[(Int, Int), List[(Int)]], lst: List[(Int, Int)]): Map[(Int, Int), List[(Int)]] = lst match {
  //   case h :: t => parseHelper2(num, board + (h -> board(h).filterNot(x => x==num)), t)
  //   case Nil => board
  // }
}