open Assert

let shared_suite : suite =
  [ Test ("Dbernhard tests", Dbernhard.dbernhard_tests)
  ; Test ("Nicdard tests", Nicdard.nicdard_tests)
  ; Test ("thbwd tests", Thbwd.tests)
  ]
