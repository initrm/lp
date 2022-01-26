task TaskOne {
  delete "error.logs.old"
  rename "error.logs.new" "error.logs.old"
}

task TaskTwo {
  backup "warning.logs.old" "backup-warning.logs.old"
}

task TaskThree {
  merge "success.logs.old" "success.logs.new" "success.logs.all"
  delete "success.logs.old"
  delete "success.logs.new"
}