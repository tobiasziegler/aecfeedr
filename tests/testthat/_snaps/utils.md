# replace_zeroes_with_na works

    Code
      replace_zeroes_with_na(x, contest_id, pollingplace_id)
    Output
      # A tibble: 4 x 4
        contest_id pollingplace_id candidate_id votes
             <int>           <int>        <int> <int>
      1          1               1            1    NA
      2          1               1            2    NA
      3          1               2            1   100
      4          1               2            2     0

# replace_zeroes_with_na handles empty results

    Code
      replace_zeroes_with_na(x, contest_id, pollingplace_id)
    Output
      # A tibble: 0 x 4
      # ... with 4 variables: contest_id <???>, pollingplace_id <???>,
      #   candidate_id <???>, votes <???>

