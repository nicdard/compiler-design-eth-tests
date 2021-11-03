open Assert


let shared_suite : suite = 
  [ Test ("dbernhard", Gradedtests.executed Dbernhard.dbernhard_tests)
  ; Test ("nicdard", Nicdard.executed Nicdard.nicdard_tests)
  ; Test ("haenniro", Haenniro.executed Haenniro.my_tests)
  ]
