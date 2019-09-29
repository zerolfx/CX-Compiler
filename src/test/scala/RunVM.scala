object RunVM extends App {
  val vm = new PCodeVM

  vm.from_text("""JMP   0   13
                 |JMP   0    2
                 |INT   0    3
                 |LOD   1    3
                 |LIT   0    1
                 |OPR   0    3
                 |STO   1    3
                 |LOD   1    3
                 |LIT   0    0
                 |OPR   0   12
                 |JPC   0   12
                 |CAL   1    2
                 |OPR   0    0
                 |INT   0    4
                 |LIT   0    3
                 |STO   0    3
                 |CAL   0    2
                 |OPR   0    0""".stripMargin)
}
