open Assert


let shared_suite : suite = 
  [ Test ("dbernhard", Gradedtests.executed Dbernhard.dbernhard_tests)
  ; Test ("nicdard", Nicdard.executed Nicdard.nicdard_tests)
  ; Test ("haenniro", Gradedtests.executed_c_link Haenniro.my_tests)
  ; Test ("zikai", Gradedtests.executed Zikai.zikai_tests)
  ]
