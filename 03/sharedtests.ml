open Assert


let shared_suite : suite = [
    Test ("dbernhard", Gradedtests.executed Dbernhard.dbernhard_tests)
]
