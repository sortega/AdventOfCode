package advent

object Day6 {

  sealed trait Action
  case object TurnOn extends Action
  case object Toggle extends Action
  case object TurnOff extends Action

  case class Area(from: (Int, Int), to: (Int, Int)) {
    def forEach(block: (Int, Int) => Unit): Unit = {
      for (row <- from._1 to to._1; col <- from._2 to to._2) {
        block(row, col)
      }
    }
  }

  case class Command(action: Action, area: Area)

  val CommandPattern = """(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)""".r

  private def parseCommand(line: String): Command = line match {
    case CommandPattern(action, row1, col1, row2, col2) =>
      Command(parseAction(action), Area((row1.toInt, col1.toInt), (row2.toInt, col2.toInt)))
  }

  private def parseAction(action: String) = action match {
    case "turn on" => TurnOn
    case "toggle" => Toggle
    case "turn off" => TurnOff
  }

  class DigitalLights {
    private val matrix = Array.ofDim[Boolean](1000, 1000)

    def execute(command: Command): Unit = {
      command.area.forEach(command.action match {
        case TurnOn => turnOn
        case TurnOff => turnOff
        case Toggle => toggle
      })
    }

    def turnOn(row: Int, col: Int): Unit = { matrix(row)(col) = true }
    def turnOff(row: Int, col: Int): Unit = { matrix(row)(col) = false }
    def toggle(row: Int, col: Int): Unit = { matrix(row)(col) = !matrix(row)(col) }

    def turnedOn = matrix.map(_.count(identity)).sum
  }

  def part1(input: String): Int = {
    val commands = input.lines.map(parseCommand)
    val lights = new DigitalLights
    commands.foreach(lights.execute)
    lights.turnedOn
  }

  class AnalogLights {
    private val matrix = Array.ofDim[Int](1000, 1000)

    def execute(command: Command): Unit = {
      command.area.forEach(command.action match {
        case TurnOn => increase
        case TurnOff => decrease
        case Toggle => double
      })
    }

    def increase(row: Int, col: Int): Unit = { matrix(row)(col) += 1 }
    def decrease(row: Int, col: Int): Unit = { matrix(row)(col) = 0 max (matrix(row)(col) - 1) }
    def double(row: Int, col: Int): Unit = { matrix(row)(col) += 2 }

    def totalBrightness: Int = matrix.flatten.sum
  }

  def part2(input: String): Int = {
    val commands = input.lines.map(parseCommand)
    val lights = new AnalogLights
    commands.foreach(lights.execute)
    lights.totalBrightness
  }

  def main(args: Array[String]): Unit = {
    val input = "turn on 489,959 through 759,964\nturn off 820,516 through 871,914\nturn off 427,423 through 929,502\nturn on 774,14 through 977,877\nturn on 410,146 through 864,337\nturn on 931,331 through 939,812\nturn off 756,53 through 923,339\nturn off 313,787 through 545,979\nturn off 12,823 through 102,934\ntoggle 756,965 through 812,992\nturn off 743,684 through 789,958\ntoggle 120,314 through 745,489\ntoggle 692,845 through 866,994\nturn off 587,176 through 850,273\nturn off 674,321 through 793,388\ntoggle 749,672 through 973,965\nturn on 943,30 through 990,907\nturn on 296,50 through 729,664\nturn on 212,957 through 490,987\ntoggle 171,31 through 688,88\nturn off 991,989 through 994,998\nturn off 913,943 through 958,953\nturn off 278,258 through 367,386\ntoggle 275,796 through 493,971\nturn off 70,873 through 798,923\ntoggle 258,985 through 663,998\nturn on 601,259 through 831,486\nturn off 914,94 through 941,102\nturn off 558,161 through 994,647\nturn on 119,662 through 760,838\ntoggle 378,775 through 526,852\nturn off 384,670 through 674,972\nturn off 249,41 through 270,936\nturn on 614,742 through 769,780\nturn on 427,70 through 575,441\nturn on 410,478 through 985,753\nturn off 619,46 through 931,342\nturn on 284,55 through 768,922\nturn off 40,592 through 728,685\nturn on 825,291 through 956,950\nturn on 147,843 through 592,909\nturn off 218,675 through 972,911\ntoggle 249,291 through 350,960\nturn off 556,80 through 967,675\ntoggle 609,148 through 968,279\ntoggle 217,605 through 961,862\ntoggle 407,177 through 548,910\ntoggle 400,936 through 599,938\nturn off 721,101 through 925,455\nturn on 268,631 through 735,814\ntoggle 549,969 through 612,991\ntoggle 553,268 through 689,432\nturn off 817,668 through 889,897\ntoggle 801,544 through 858,556\ntoggle 615,729 through 832,951\nturn off 427,477 through 958,948\nturn on 164,49 through 852,946\nturn on 542,449 through 774,776\nturn off 923,196 through 980,446\ntoggle 90,310 through 718,846\nturn off 657,215 through 744,252\nturn off 800,239 through 811,712\nturn on 502,90 through 619,760\ntoggle 649,512 through 862,844\nturn off 334,903 through 823,935\nturn off 630,233 through 839,445\nturn on 713,67 through 839,865\nturn on 932,50 through 982,411\nturn off 480,729 through 984,910\nturn on 100,219 through 796,395\nturn on 758,108 through 850,950\nturn off 427,276 through 439,938\nturn on 178,284 through 670,536\ntoggle 540,27 through 625,102\nturn off 906,722 through 936,948\ntoggle 345,418 through 859,627\ntoggle 175,775 through 580,781\ntoggle 863,28 through 929,735\nturn off 824,858 through 905,973\ntoggle 752,312 through 863,425\nturn on 985,716 through 988,852\nturn off 68,504 through 763,745\ntoggle 76,209 through 810,720\nturn off 657,607 through 676,664\ntoggle 596,869 through 896,921\nturn off 915,411 through 968,945\nturn off 368,39 through 902,986\nturn on 11,549 through 393,597\nturn off 842,893 through 976,911\ntoggle 274,106 through 581,329\ntoggle 406,403 through 780,950\ntoggle 408,988 through 500,994\ntoggle 217,73 through 826,951\nturn on 917,872 through 961,911\ntoggle 394,34 through 510,572\ntoggle 424,603 through 583,626\ntoggle 106,159 through 755,738\nturn off 244,610 through 472,709\nturn on 350,265 through 884,690\nturn on 688,184 through 928,280\ntoggle 279,443 through 720,797\nturn off 615,493 through 888,610\ntoggle 118,413 through 736,632\nturn on 798,782 through 829,813\nturn off 250,934 through 442,972\nturn on 68,503 through 400,949\ntoggle 297,482 through 313,871\ntoggle 710,3 through 839,859\nturn on 125,300 through 546,888\ntoggle 482,39 through 584,159\nturn off 536,89 through 765,962\nturn on 530,518 through 843,676\nturn on 994,467 through 994,676\nturn on 623,628 through 744,927\ntoggle 704,912 through 837,983\nturn on 154,364 through 517,412\ntoggle 344,409 through 780,524\nturn off 578,740 through 725,879\nturn on 251,933 through 632,957\nturn on 827,705 through 971,789\ntoggle 191,282 through 470,929\ntoggle 324,525 through 446,867\ntoggle 534,343 through 874,971\ntoggle 550,650 through 633,980\ntoggle 837,404 through 881,915\ntoggle 338,881 through 845,905\nturn on 469,462 through 750,696\nturn on 741,703 through 892,870\nturn off 570,215 through 733,562\nturn on 445,576 through 870,775\nturn on 466,747 through 554,878\nturn off 820,453 through 868,712\nturn off 892,706 through 938,792\nturn off 300,238 through 894,746\nturn off 306,44 through 457,444\nturn off 912,569 through 967,963\ntoggle 109,756 through 297,867\nturn on 37,546 through 41,951\nturn on 321,637 through 790,910\ntoggle 66,50 through 579,301\ntoggle 933,221 through 933,791\nturn on 486,676 through 878,797\nturn on 417,231 through 556,317\ntoggle 904,468 through 981,873\nturn on 417,675 through 749,712\nturn on 692,371 through 821,842\ntoggle 324,73 through 830,543\nturn on 912,490 through 977,757\nturn off 634,872 through 902,949\ntoggle 266,779 through 870,798\nturn on 772,982 through 990,996\nturn off 607,46 through 798,559\nturn on 295,602 through 963,987\nturn on 657,86 through 944,742\nturn off 334,639 through 456,821\nturn off 997,667 through 997,670\nturn off 725,832 through 951,945\nturn off 30,120 through 952,984\nturn on 860,965 through 917,976\ntoggle 471,997 through 840,998\nturn off 319,307 through 928,504\ntoggle 823,631 through 940,908\ntoggle 969,984 through 981,993\nturn off 691,319 through 865,954\ntoggle 911,926 through 938,929\nturn on 953,937 through 968,991\ntoggle 914,643 through 975,840\nturn on 266,982 through 436,996\nturn off 101,896 through 321,932\nturn off 193,852 through 751,885\nturn off 576,532 through 863,684\nturn on 761,456 through 940,783\nturn on 20,290 through 398,933\nturn off 435,335 through 644,652\nturn on 830,569 through 905,770\nturn off 630,517 through 905,654\nturn on 664,53 through 886,976\ntoggle 275,416 through 408,719\nturn on 370,621 through 515,793\nturn on 483,373 through 654,749\nturn on 656,786 through 847,928\nturn off 532,752 through 945,974\ntoggle 301,150 through 880,792\nturn off 951,488 through 958,952\nturn on 207,729 through 882,828\ntoggle 694,532 through 973,961\ntoggle 676,639 through 891,802\nturn off 653,6 through 905,519\ntoggle 391,109 through 418,312\nturn on 877,423 through 957,932\nturn on 340,145 through 563,522\nturn off 978,467 through 988,895\nturn off 396,418 through 420,885\nturn off 31,308 through 816,316\nturn on 107,675 through 758,824\nturn on 61,82 through 789,876\nturn on 750,743 through 754,760\ntoggle 88,733 through 736,968\nturn off 754,349 through 849,897\ntoggle 157,50 through 975,781\nturn off 230,231 through 865,842\nturn off 516,317 through 630,329\nturn off 697,820 through 829,903\nturn on 218,250 through 271,732\ntoggle 56,167 through 404,431\ntoggle 626,891 through 680,927\ntoggle 370,207 through 791,514\ntoggle 860,74 through 949,888\nturn on 416,527 through 616,541\nturn off 745,449 through 786,908\nturn on 485,554 through 689,689\nturn on 586,62 through 693,141\ntoggle 506,759 through 768,829\nturn on 473,109 through 929,166\nturn on 760,617 through 773,789\ntoggle 595,683 through 618,789\nturn off 210,775 through 825,972\ntoggle 12,426 through 179,982\nturn on 774,539 through 778,786\nturn on 102,498 through 121,807\nturn off 706,897 through 834,965\nturn off 678,529 through 824,627\nturn on 7,765 through 615,870\nturn off 730,872 through 974,943\nturn off 595,626 through 836,711\nturn off 215,424 through 841,959\ntoggle 341,780 through 861,813\ntoggle 507,503 through 568,822\nturn on 252,603 through 349,655\ntoggle 93,521 through 154,834\nturn on 565,682 through 951,954\nturn on 544,318 through 703,418\ntoggle 756,953 through 891,964\nturn on 531,123 through 856,991\nturn on 148,315 through 776,559\nturn off 925,835 through 963,971\nturn on 895,944 through 967,964\nturn off 102,527 through 650,747\ntoggle 626,105 through 738,720\nturn off 160,75 through 384,922\ntoggle 813,724 through 903,941\nturn on 207,107 through 982,849\ntoggle 750,505 through 961,697\ntoggle 105,410 through 885,819\nturn on 226,104 through 298,283\nturn off 224,604 through 508,762\nturn on 477,368 through 523,506\nturn off 477,901 through 627,936\nturn off 887,131 through 889,670\nturn on 896,994 through 938,999\ntoggle 401,580 through 493,728\ntoggle 987,184 through 991,205\nturn on 821,643 through 882,674\ntoggle 784,940 through 968,959\nturn off 251,293 through 274,632\nturn off 339,840 through 341,844\nturn off 675,351 through 675,836\ntoggle 918,857 through 944,886\ntoggle 70,253 through 918,736\nturn off 612,604 through 772,680\nturn off 277,40 through 828,348\ntoggle 692,139 through 698,880\ntoggle 124,446 through 883,453\ntoggle 969,932 through 990,945\ntoggle 855,692 through 993,693\ntoggle 722,472 through 887,899\ntoggle 978,149 through 985,442\ntoggle 837,540 through 916,889\nturn off 612,2 through 835,82\ntoggle 560,767 through 878,856\nturn on 461,734 through 524,991\ntoggle 206,824 through 976,912\nturn on 826,610 through 879,892\nturn on 577,699 through 956,933\nturn off 9,250 through 50,529\nturn off 77,657 through 817,677\nturn on 68,419 through 86,426\nturn on 991,720 through 992,784\nturn on 668,20 through 935,470\nturn off 133,418 through 613,458\nturn off 487,286 through 540,328\ntoggle 247,874 through 840,955\ntoggle 301,808 through 754,970\nturn off 34,194 through 578,203\nturn off 451,49 through 492,921\nturn on 907,256 through 912,737\nturn off 479,305 through 702,587\nturn on 545,583 through 732,749\ntoggle 11,16 through 725,868\nturn on 965,343 through 986,908\nturn on 674,953 through 820,965\ntoggle 398,147 through 504,583\nturn off 778,194 through 898,298\nturn on 179,140 through 350,852\nturn off 241,118 through 530,832\nturn off 41,447 through 932,737\nturn off 820,663 through 832,982\nturn on 550,460 through 964,782\nturn on 31,760 through 655,892\ntoggle 628,958 through 811,992"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
