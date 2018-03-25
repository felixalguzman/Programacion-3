structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | INITIAL
    structure UserDeclarations = 
      struct

type pos = int
type lexresult = Tokens.token
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val numComment = ref 0
val stringToken = ref ""
val stringStrPos = ref 0
fun err(p1,p2) = ErrorMsg.error p1
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\^@",#"\b",2),
(#"\v",#"\f",2),
(#"\^N",#"\^_",2),
(#"!",#"%",2),
(#"'",#"'",2),
(#"?",#"Z",2),
(#"\\",#"\\",2),
(#"^",#"`",2),
(#"c",#"c",2),
(#"g",#"h",2),
(#"j",#"k",2),
(#"m",#"m",2),
(#"p",#"s",2),
(#"u",#"u",2),
(#"x",#"z",2),
(#"~",#"\255",2),
(#"\t",#"\t",3),
(#" ",#" ",3),
(#"\n",#"\n",4),
(#"\r",#"\r",5),
(#"&",#"&",6),
(#"(",#"(",7),
(#")",#")",8),
(#"*",#"*",9),
(#"+",#"+",10),
(#",",#",",11),
(#"-",#"-",12),
(#".",#".",13),
(#"/",#"/",14),
(#"0",#"9",15),
(#":",#":",16),
(#";",#";",17),
(#"<",#"<",18),
(#"=",#"=",19),
(#">",#">",20),
(#"[",#"[",21),
(#"]",#"]",22),
(#"a",#"a",23),
(#"b",#"b",24),
(#"d",#"d",25),
(#"e",#"e",26),
(#"f",#"f",27),
(#"i",#"i",28),
(#"l",#"l",29),
(#"n",#"n",30),
(#"o",#"o",31),
(#"t",#"t",32),
(#"v",#"v",33),
(#"w",#"w",34),
(#"{",#"{",35),
(#"|",#"|",36),
(#"}",#"}",37)], [42]), ([(#"\^@",#"\b",89),
(#"\v",#"\f",89),
(#"\^N",#"\^_",89),
(#"!",#"!",89),
(#"#",#"%",89),
(#"'",#"'",89),
(#"?",#"@",89),
(#"\\",#"\\",89),
(#"^",#"`",89),
(#"~",#"\255",89),
(#"\t",#"\t",90),
(#" ",#" ",90),
(#"\n",#"\n",4),
(#"\r",#"\r",91),
(#"\"",#"\"",92),
(#"&",#"&",93),
(#"(",#"(",94),
(#")",#")",95),
(#"*",#"*",96),
(#"+",#"+",97),
(#",",#",",98),
(#"-",#"-",99),
(#".",#".",100),
(#"/",#"/",101),
(#"0",#"9",102),
(#":",#":",103),
(#";",#";",104),
(#"<",#"<",105),
(#"=",#"=",106),
(#">",#">",107),
(#"A",#"Z",108),
(#"c",#"c",108),
(#"g",#"h",108),
(#"j",#"k",108),
(#"m",#"m",108),
(#"p",#"s",108),
(#"u",#"u",108),
(#"x",#"z",108),
(#"[",#"[",109),
(#"]",#"]",110),
(#"a",#"a",111),
(#"b",#"b",112),
(#"d",#"d",113),
(#"e",#"e",114),
(#"f",#"f",115),
(#"i",#"i",116),
(#"l",#"l",117),
(#"n",#"n",118),
(#"o",#"o",119),
(#"t",#"t",120),
(#"v",#"v",121),
(#"w",#"w",122),
(#"{",#"{",123),
(#"|",#"|",124),
(#"}",#"}",125)], [42]), ([], [47, 48]), ([(#"\t",#"\t",88),
(#" ",#" ",88)], [1, 47, 48]), ([(#"\n",#"\n",4),
(#"\r",#"\r",4)], [0]), ([(#"\n",#"\n",4),
(#"\r",#"\r",4)], [0, 47, 48]), ([], [21, 47, 48]), ([], [38, 47, 48]), ([], [37, 47, 48]), ([(#"/",#"/",87)], [29, 47, 48]), ([], [31, 47, 48]), ([], [41, 47, 48]), ([], [30, 47, 48]), ([], [32, 47, 48]), ([(#"*",#"*",86)], [28, 47, 48]), ([(#"0",#"9",85)], [42, 47, 48]), ([(#"=",#"=",84)], [40, 47, 48]), ([], [39, 47, 48]), ([(#"=",#"=",82),
(#">",#">",83)], [25, 47, 48]), ([], [27, 47, 48]), ([(#"=",#"=",81)], [23, 47, 48]), ([], [36, 47, 48]), ([], [35, 47, 48]), ([(#"r",#"r",77)], [47, 48]), ([(#"r",#"r",73)], [47, 48]), ([(#"o",#"o",72)], [47, 48]), ([(#"l",#"l",67),
(#"n",#"n",68)], [47, 48]), ([(#"o",#"o",58),
(#"u",#"u",59)], [47, 48]), ([(#"f",#"f",56),
(#"n",#"n",57)], [47, 48]), ([(#"e",#"e",54)], [47, 48]), ([(#"i",#"i",52)], [47, 48]), ([(#"f",#"f",51)], [47, 48]), ([(#"h",#"h",44),
(#"o",#"o",45),
(#"y",#"y",46)], [47, 48]), ([(#"a",#"a",42)], [47, 48]), ([(#"h",#"h",38)], [47, 48]), ([], [34, 47, 48]), ([], [20, 47, 48]), ([], [33, 47, 48]), ([(#"i",#"i",39)], []), ([(#"l",#"l",40)], []), ([(#"e",#"e",41)], []), ([], [14]), ([(#"r",#"r",43)], []), ([], [2]), ([(#"e",#"e",49)], []), ([], [12]), ([(#"p",#"p",47)], []), ([(#"e",#"e",48)], []), ([], [3]), ([(#"n",#"n",50)], []), ([], [16]), ([], [6]), ([(#"l",#"l",53)], []), ([], [9]), ([(#"t",#"t",55)], []), ([], [10]), ([], [17]), ([], [8]), ([(#"r",#"r",66)], []), ([(#"n",#"n",60)], []), ([(#"c",#"c",61)], []), ([(#"t",#"t",62)], []), ([(#"i",#"i",63)], []), ([(#"o",#"o",64)], []), ([(#"n",#"n",65)], []), ([], [4]), ([], [13]), ([(#"s",#"s",70)], []), ([(#"d",#"d",69)], []), ([], [7]), ([(#"e",#"e",71)], []), ([], [15]), ([], [11]), ([(#"e",#"e",74)], []), ([(#"a",#"a",75)], []), ([(#"k",#"k",76)], []), ([], [5]), ([(#"r",#"r",78)], []), ([(#"a",#"a",79)], []), ([(#"y",#"y",80)], []), ([], [18]), ([], [22]), ([], [24]), ([], [26]), ([], [19]), ([(#"0",#"9",85)], [42]), ([], [45]), ([], [46]), ([(#"\t",#"\t",88),
(#" ",#" ",88)], [1]), ([], [48]), ([(#"\t",#"\t",88),
(#" ",#" ",88)], [1, 48]), ([(#"\n",#"\n",4),
(#"\r",#"\r",4)], [0, 48]), ([(#"\^@",#"!",170),
(#"#",#"\255",170),
(#"\"",#"\"",171)], [48]), ([], [21, 48]), ([], [38, 48]), ([], [37, 48]), ([], [29, 48]), ([], [31, 48]), ([], [41, 48]), ([], [30, 48]), ([], [32, 48]), ([(#"*",#"*",86)], [28, 48]), ([(#"0",#"9",85)], [42, 48]), ([(#"=",#"=",84)], [40, 48]), ([], [39, 48]), ([(#"=",#"=",82),
(#">",#">",83)], [25, 48]), ([], [27, 48]), ([(#"=",#"=",81)], [23, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [43, 48]), ([], [36, 48]), ([], [35, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"q",126),
(#"s",#"z",126),
(#"r",#"r",166)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"q",126),
(#"s",#"z",126),
(#"r",#"r",162)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"n",126),
(#"p",#"z",126),
(#"o",#"o",161)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"k",126),
(#"m",#"m",126),
(#"o",#"z",126),
(#"l",#"l",156),
(#"n",#"n",157)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"n",126),
(#"p",#"t",126),
(#"v",#"z",126),
(#"o",#"o",147),
(#"u",#"u",148)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"e",126),
(#"g",#"m",126),
(#"o",#"z",126),
(#"f",#"f",145),
(#"n",#"n",146)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"d",126),
(#"f",#"z",126),
(#"e",#"e",143)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"h",126),
(#"j",#"z",126),
(#"i",#"i",141)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"e",126),
(#"g",#"z",126),
(#"f",#"f",140)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"g",126),
(#"i",#"n",126),
(#"p",#"x",126),
(#"z",#"z",126),
(#"h",#"h",133),
(#"o",#"o",134),
(#"y",#"y",135)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"b",#"z",126),
(#"a",#"a",131)], [43, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"g",126),
(#"i",#"z",126),
(#"h",#"h",127)], [43, 48]), ([], [34, 48]), ([], [20, 48]), ([], [33, 48]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"h",126),
(#"j",#"z",126),
(#"i",#"i",128)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"k",126),
(#"m",#"z",126),
(#"l",#"l",129)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"d",126),
(#"f",#"z",126),
(#"e",#"e",130)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [14, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"q",126),
(#"s",#"z",126),
(#"r",#"r",132)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [2, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"d",126),
(#"f",#"z",126),
(#"e",#"e",138)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [12, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"o",126),
(#"q",#"z",126),
(#"p",#"p",136)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"d",126),
(#"f",#"z",126),
(#"e",#"e",137)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [3, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"m",126),
(#"o",#"z",126),
(#"n",#"n",139)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [16, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [6, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"k",126),
(#"m",#"z",126),
(#"l",#"l",142)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [9, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"s",126),
(#"u",#"z",126),
(#"t",#"t",144)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [10, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [17, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [8, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"q",126),
(#"s",#"z",126),
(#"r",#"r",155)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"m",126),
(#"o",#"z",126),
(#"n",#"n",149)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"b",126),
(#"d",#"z",126),
(#"c",#"c",150)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"s",126),
(#"u",#"z",126),
(#"t",#"t",151)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"h",126),
(#"j",#"z",126),
(#"i",#"i",152)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"n",126),
(#"p",#"z",126),
(#"o",#"o",153)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"m",126),
(#"o",#"z",126),
(#"n",#"n",154)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [4, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [13, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"r",126),
(#"t",#"z",126),
(#"s",#"s",159)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"c",126),
(#"e",#"z",126),
(#"d",#"d",158)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [7, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"d",126),
(#"f",#"z",126),
(#"e",#"e",160)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [15, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [11, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"d",126),
(#"f",#"z",126),
(#"e",#"e",163)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"b",#"z",126),
(#"a",#"a",164)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"j",126),
(#"l",#"z",126),
(#"k",#"k",165)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [5, 43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"q",126),
(#"s",#"z",126),
(#"r",#"r",167)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"b",#"z",126),
(#"a",#"a",168)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"x",126),
(#"z",#"z",126),
(#"y",#"y",169)], [43]), ([(#"0",#"9",126),
(#"A",#"Z",126),
(#"_",#"_",126),
(#"a",#"z",126)], [18, 43]), ([(#"\^@",#"!",170),
(#"#",#"\255",170),
(#"\"",#"\"",171)], []), ([], [44])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(yypos,yypos+3)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TYPE(yypos,yypos+4)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FUNCTION(yypos,yypos+8)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BREAK(yypos,yypos+5)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OF(yypos,yypos+2)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.END(yypos,yypos+3)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IN(yypos,yypos+2)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NIL(yypos,yypos+3)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LET(yypos,yypos+3)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(yypos,yypos+2)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TO(yypos,yypos+2)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FOR(yypos,yypos+3)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(yypos,yypos+5)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(yypos,yypos+4)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(yypos,yypos+4)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(yypos,yypos+2)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ARRAY(yypos,yypos+5)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(yypos,yypos+2)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos,yypos+1)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos,yypos+1)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GE(yypos,yypos+2)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos,yypos+2)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LE(yypos,yypos+2)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos,yypos+1)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos,yypos+2)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos,yypos+1)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos,yypos+1)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos,yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos,yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos,yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos,yypos+1)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos, yypos+1)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos, yypos+1)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACK(yypos, yypos+1)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACK(yypos, yypos+1)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos, yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos, yypos+1)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos, yypos+1)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos, yypos+1)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos, yypos+1)))
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos+size yytext))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID (yytext, yypos, yypos+size yytext))
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.STRING (yytext, yypos, yypos+size yytext))
      end
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; continue()))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of COMMENT => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
