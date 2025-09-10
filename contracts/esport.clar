
;; title: esport
;; version:
;; summary:
;; description:

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-tournament-full (err u103))
(define-constant err-tournament-not-active (err u104))
(define-constant err-tournament-not-ended (err u105))
(define-constant err-invalid-tournament-id (err u106))
(define-constant err-invalid-prize-distribution (err u107))
(define-constant err-insufficient-funds (err u108))
(define-constant err-not-winner (err u109))
(define-constant err-prize-already-claimed (err u110))
(define-constant err-not-authorized (err u111))

(define-data-var tournament-counter uint u0)

(define-constant err-leaderboard-not-found (err u117))
(define-constant err-invalid-leaderboard-entry (err u118))

(define-data-var global-leaderboard-size uint u0)

(define-map global-leaderboard
  { rank: uint }
  {
    player: principal,
    rating: uint,
    tournaments-completed: uint,
    total-prize-money: uint,
    win-rate: uint
  }
)

(define-map player-leaderboard-position
  { player: principal }
  {
    current-rank: uint,
    previous-rank: uint,
    rating: uint,
    last-tournament-date: uint
  }
)

(define-map tournaments
  { tournament-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    max-participants: uint,
    entry-fee: uint,
    total-prize-pool: uint,
    registration-open: bool,
    tournament-ended: bool,
    participant-count: uint,
    nft-required: (optional principal)
  }
)

(define-map tournament-participants
  { tournament-id: uint, participant: principal }
  { registered: bool, position: (optional uint), prize-claimed: bool }
)

(define-map tournament-winners
  { tournament-id: uint, position: uint }
  { winner: principal, prize-amount: uint }
)

(define-map user-nfts
  { user: principal, nft-contract: principal }
  { has-nft: bool }
)

(define-read-only (get-tournament-counter)
  (var-get tournament-counter)
)

(define-read-only (get-tournament (tournament-id uint))
  (map-get? tournaments { tournament-id: tournament-id })
)

(define-read-only (get-participant-status (tournament-id uint) (participant principal))
  (map-get? tournament-participants { tournament-id: tournament-id, participant: participant })
)

(define-read-only (get-tournament-winner (tournament-id uint) (position uint))
  (map-get? tournament-winners { tournament-id: tournament-id, position: position })
)

(define-read-only (check-user-nft (user principal) (nft-contract principal))
  (default-to 
    { has-nft: false }
    (map-get? user-nfts { user: user, nft-contract: nft-contract })
  )
)

(define-public (create-tournament (name (string-ascii 50)) (description (string-ascii 200)) (max-participants uint) (entry-fee uint) (nft-required (optional principal)))
  (let ((tournament-id (+ (var-get tournament-counter) u1)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set tournaments
      { tournament-id: tournament-id }
      {
        name: name,
        description: description,
        max-participants: max-participants,
        entry-fee: entry-fee,
        total-prize-pool: u0,
        registration-open: true,
        tournament-ended: false,
        participant-count: u0,
        nft-required: nft-required
      }
    )
    (var-set tournament-counter tournament-id)
    (ok tournament-id)
  )
)

(define-public (register-for-tournament (tournament-id uint))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id))
    (participant-status (map-get? tournament-participants { tournament-id: tournament-id, participant: tx-sender }))
  )
    (asserts! (get registration-open tournament) err-tournament-not-active)
    (asserts! (< (get participant-count tournament) (get max-participants tournament)) err-tournament-full)
    (asserts! (is-none participant-status) err-already-registered)
    
    (match (get nft-required tournament)
      nft-contract (asserts! (get has-nft (check-user-nft tx-sender nft-contract)) err-not-authorized)
      true
    )
    
    (try! (stx-transfer? (get entry-fee tournament) tx-sender (as-contract tx-sender)))
    
    (map-set tournament-participants
      { tournament-id: tournament-id, participant: tx-sender }
      { registered: true, position: none, prize-claimed: false }
    )
    
    (map-set tournaments
      { tournament-id: tournament-id }
      (merge tournament {
        participant-count: (+ (get participant-count tournament) u1),
        total-prize-pool: (+ (get total-prize-pool tournament) (get entry-fee tournament))
      })
    )
    
    (ok true)
  )
)

(define-public (close-tournament-registration (tournament-id uint))
  (let ((tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get registration-open tournament) err-tournament-not-active)
    
    (map-set tournaments
      { tournament-id: tournament-id }
      (merge tournament { registration-open: false })
    )
    
    (ok true)
  )
)

(define-public (end-tournament (tournament-id uint))
  (let ((tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get registration-open tournament)) err-tournament-not-active)
    (asserts! (not (get tournament-ended tournament)) err-tournament-not-active)
    
    (map-set tournaments
      { tournament-id: tournament-id }
      (merge tournament { tournament-ended: true })
    )
    
    (ok true)
  )
)

(define-public (set-tournament-winners (tournament-id uint) (winners (list 10 { position: uint, winner: principal, prize-amount: uint })))
  (let ((tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get registration-open tournament)) err-tournament-not-active)
    (asserts! (get tournament-ended tournament) err-tournament-not-ended)
    
    (fold check-and-set-winner winners (ok true))
  )
)

(define-private (check-and-set-winner (winner-data { position: uint, winner: principal, prize-amount: uint }) (previous-result (response bool uint)))
  (begin
    ;; (asserts! (is-ok previous-result) (unwrap-err! previous-result err-invalid-prize-distribution))
    
    (let (
      (position (get position winner-data))
      (winner (get winner winner-data))
      (prize-amount (get prize-amount winner-data))
    )
      (map-set tournament-winners
        { tournament-id: (var-get tournament-counter), position: position }
        { winner: winner, prize-amount: prize-amount }
      )
      
      (map-set tournament-participants
        { tournament-id: (var-get tournament-counter), participant: winner }
        { registered: true, position: (some position), prize-claimed: false }
      )
      
      (ok true)
    )
  )
)

(define-public (claim-prize (tournament-id uint))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id))
    (participant-data (unwrap! (map-get? tournament-participants { tournament-id: tournament-id, participant: tx-sender }) err-not-registered))
  )
    (asserts! (get tournament-ended tournament) err-tournament-not-ended)
    (asserts! (is-some (get position participant-data)) err-not-winner)
    (asserts! (not (get prize-claimed participant-data)) err-prize-already-claimed)
    
    (let (
      (position (unwrap! (get position participant-data) err-not-winner))
      (winner-data (unwrap! (map-get? tournament-winners { tournament-id: tournament-id, position: position }) err-not-winner))
    )
      (asserts! (is-eq (get winner winner-data) tx-sender) err-not-winner)
      
      (try! (as-contract (stx-transfer? (get prize-amount winner-data) tx-sender tx-sender)))
      
      (map-set tournament-participants
        { tournament-id: tournament-id, participant: tx-sender }
        (merge participant-data { prize-claimed: true })
      )
      
      (ok true)
    )
  )
)

(define-public (register-nft (user principal) (nft-contract principal) (has-nft bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set user-nfts
      { user: user, nft-contract: nft-contract }
      { has-nft: has-nft }
    )
    
    (ok true)
  )
)

(define-public (withdraw-remaining-funds (tournament-id uint))
  (let ((tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get tournament-ended tournament) err-tournament-not-ended)
    
    (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
      (try! (as-contract (stx-transfer? contract-balance contract-owner contract-owner)))
      (ok contract-balance)
    )
  )
)


(define-map player-statistics
  { player: principal }
  {
    tournaments-played: uint,
    tournaments-won: uint,
    total-earnings: uint,
    best-position: uint
  }
)

(define-read-only (get-player-stats (player principal))
  (default-to
    { tournaments-played: u0, tournaments-won: u0, total-earnings: u0, best-position: u999 }
    (map-get? player-statistics { player: player })
  )
)

(define-public (update-player-statistics (tournament-id uint) (player principal) (position uint) (earnings uint))
  (let (
    (current-stats (get-player-stats player))
    (new-best-position (if (< position (get best-position current-stats))
      position
      (get best-position current-stats)))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set player-statistics
      { player: player }
      {
        tournaments-played: (+ (get tournaments-played current-stats) u1),
        tournaments-won: (if (is-eq position u1) 
          (+ (get tournaments-won current-stats) u1)
          (get tournaments-won current-stats)),
        total-earnings: (+ (get total-earnings current-stats) earnings),
        best-position: new-best-position
      }
    )
    (ok true)
  )
)


(define-public (get-tournament-participants (tournament-id uint) (participant principal))
  (let ((participants (map-get? tournament-participants { tournament-id: tournament-id, participant: participant })))
    (if (is-some participants)
      (ok participants)
      (err err-not-registered)
    )
  )
)


(define-public (get-tournament-winner-advance (tournament-id uint) (position uint))
  (let ((winner (map-get? tournament-winners { tournament-id: tournament-id, position: position })))
    (if (is-some winner)
      (ok winner)
      (err err-not-registered)
    )
  )
)(define-map tournament-sponsors
  { tournament-id: uint, sponsor: principal }
  { amount: uint }
)

(define-read-only (get-sponsor-contribution (tournament-id uint) (sponsor principal))
  (default-to
    { amount: u0 }
    (map-get? tournament-sponsors { tournament-id: tournament-id, sponsor: sponsor })
  )
)

(define-public (sponsor-tournament (tournament-id uint) (amount uint))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id))
    (current-contribution (get amount (get-sponsor-contribution tournament-id tx-sender)))
  )
    (asserts! (get registration-open tournament) err-tournament-not-active)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set tournament-sponsors
      { tournament-id: tournament-id, sponsor: tx-sender }
      { amount: (+ current-contribution amount) }
    )
    
    (map-set tournaments
      { tournament-id: tournament-id }
      (merge tournament {
        total-prize-pool: (+ (get total-prize-pool tournament) amount)
      })
    )
    (ok true)
  )
)


(define-constant err-invalid-match-id (err u112))
(define-constant err-match-already-played (err u113))
(define-constant err-not-match-participant (err u114))
(define-constant err-bracket-not-generated (err u115))
(define-constant err-bracket-already-exists (err u116))

(define-data-var match-counter uint u0)

(define-map tournament-brackets
  { tournament-id: uint }
  {
    total-rounds: uint,
    current-round: uint,
    bracket-generated: bool,
    matches-per-round: (list 10 uint)
  }
)

(define-map bracket-matches
  { tournament-id: uint, match-id: uint }
  {
    round: uint,
    player1: (optional principal),
    player2: (optional principal),
    winner: (optional principal),
    match-completed: bool,
    next-match-id: (optional uint)
  }
)

(define-map player-bracket-position
  { tournament-id: uint, player: principal }
  {
    current-match-id: (optional uint),
    eliminated: bool,
    elimination-round: (optional uint)
  }
)

(define-read-only (get-tournament-bracket (tournament-id uint))
  (map-get? tournament-brackets { tournament-id: tournament-id })
)

(define-read-only (get-bracket-match (tournament-id uint) (match-id uint))
  (map-get? bracket-matches { tournament-id: tournament-id, match-id: match-id })
)

(define-read-only (get-player-bracket-status (tournament-id uint) (player principal))
  (map-get? player-bracket-position { tournament-id: tournament-id, player: player })
)

(define-public (generate-tournament-bracket (tournament-id uint))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) (err u100)))
    (existing-bracket (map-get? tournament-brackets { tournament-id: tournament-id }))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get registration-open tournament)) err-tournament-not-active)
    (asserts! (is-none existing-bracket) err-bracket-already-exists)
    
    (let (
      (participant-count (get participant-count tournament))
      (total-rounds (calculate-rounds participant-count))
      (first-round-matches (/ participant-count u2))
    )
      (map-set tournament-brackets
        { tournament-id: tournament-id }
        {
          total-rounds: total-rounds,
          current-round: u1,
          bracket-generated: true,
          matches-per-round: (list first-round-matches)
        }
      )
      
      ;; (try! (create-first-round-matches tournament-id participant-count))
      (ok true)
    )
  )
)
(define-private (calculate-rounds (participants uint))
  (if (<= participants u2) u1
    (if (<= participants u4) u2
      (if (<= participants u8) u3
        (if (<= participants u16) u4
          (if (<= participants u32) u5 u6)))))
)

(define-private (create-first-round-matches (tournament-id uint) (participant-count uint))
  (begin
    (let ((matches-needed (/ participant-count u2)))
      (fold create-match-fold 
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16)
        { tournament-id: tournament-id, matches-created: u0, matches-needed: matches-needed }
      )
    )
    (ok true)
  )
)

(define-private (create-match-fold 
  (index uint) 
  (data { tournament-id: uint, matches-created: uint, matches-needed: uint })
)
  (if (< (get matches-created data) (get matches-needed data))
    (begin
      (var-set match-counter (+ (var-get match-counter) u1))
      (map-set bracket-matches
        { tournament-id: (get tournament-id data), match-id: (var-get match-counter) }
        {
          round: u1,
          player1: none,
          player2: none,
          winner: none,
          match-completed: false,
          next-match-id: none
        }
      )
      (merge data { matches-created: (+ (get matches-created data) u1) })
    )
    data
  )
)

(define-public (assign-players-to-bracket (tournament-id uint) (player-assignments (list 32 { player: principal, match-id: uint, position: uint, tournament-id: uint })))
  (let ((bracket (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-bracket-not-generated)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get bracket-generated bracket) err-bracket-not-generated)
    
    (try! (fold assign-player-to-match player-assignments (ok tournament-id)))
    (ok true)
  )
)

(define-private (assign-player-to-match (assignment { player: principal, match-id: uint, position: uint, tournament-id: uint }) (previous-result (response uint uint)))
  (let (
    (player (get player assignment))
    (match-id (get match-id assignment))
    (position (get position assignment))
    (tournament-id (get tournament-id assignment))
    (current-match (unwrap! (map-get? bracket-matches { tournament-id: tournament-id, match-id: match-id }) (err u0)))
  )
    (if (is-eq position u1)
      (begin
        (map-set bracket-matches
          { tournament-id: tournament-id, match-id: match-id }
          (merge current-match { player1: (some player) })
        )
        (map-set player-bracket-position
          { tournament-id: tournament-id, player: player }
          {
            current-match-id: (some match-id),
            eliminated: false,
            elimination-round: none
          }
        )
        (ok tournament-id)
      )
      (begin
        (map-set bracket-matches
          { tournament-id: tournament-id, match-id: match-id }
          (merge current-match { player2: (some player) })
        )
        (map-set player-bracket-position
          { tournament-id: tournament-id, player: player }
          {
            current-match-id: (some match-id),
            eliminated: false,
            elimination-round: none
          }
        )
        (ok tournament-id)
      )
    )
  )
)

(define-public (report-match-result (tournament-id uint) (match-id uint) (winner principal))
  (let (
    (bracket (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-bracket-not-generated))
    (match-data (unwrap! (map-get? bracket-matches { tournament-id: tournament-id, match-id: match-id }) err-invalid-match-id))
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get match-completed match-data)) err-match-already-played)
    (asserts! (not (get registration-open tournament)) err-tournament-not-active)
    
    (let (
      (player1 (get player1 match-data))
      (player2 (get player2 match-data))
      (loser (if (is-eq (some winner) player1) player2 player1))
    )
      (asserts! (or (is-eq (some winner) player1) (is-eq (some winner) player2)) err-not-match-participant)
      
      (map-set bracket-matches
        { tournament-id: tournament-id, match-id: match-id }
        (merge match-data { winner: (some winner), match-completed: true })
      )
      
      (match loser
        eliminated-player (map-set player-bracket-position
          { tournament-id: tournament-id, player: eliminated-player }
          {
            current-match-id: none,
            eliminated: true,
            elimination-round: (some (get round match-data))
          }
        )
        true
      )
      
      (ok true)
    )
  )
)

(define-public (advance-bracket-round (tournament-id uint))
  (let (
    (bracket (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-bracket-not-generated))
    (current-round (get current-round bracket))
    (total-rounds (get total-rounds bracket))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< current-round total-rounds) err-tournament-not-active)
    
    (map-set tournament-brackets
      { tournament-id: tournament-id }
      (merge bracket { current-round: (+ current-round u1) })
    )
    
    (ok true)
  )
)

(define-read-only (get-round-matches (tournament-id uint) (round uint))
  (let ((bracket (map-get? tournament-brackets { tournament-id: tournament-id })))
    (if (is-some bracket)
      (ok (filter-matches-by-round tournament-id round))
      (err err-bracket-not-generated)
    )
  )
)

(define-private (filter-matches-by-round (tournament-id uint) (target-round uint))
  (list 
    (map-get? bracket-matches { tournament-id: tournament-id, match-id: u1 })
    (map-get? bracket-matches { tournament-id: tournament-id, match-id: u2 })
    (map-get? bracket-matches { tournament-id: tournament-id, match-id: u3 })
    (map-get? bracket-matches { tournament-id: tournament-id, match-id: u4 })
  )
)

(define-read-only (is-tournament-bracket-complete (tournament-id uint))
  (let ((bracket (map-get? tournament-brackets { tournament-id: tournament-id })))
    (match bracket
      bracket-data (is-eq (get current-round bracket-data) (get total-rounds bracket-data))
      false
    )
  )
)



(define-read-only (get-leaderboard-entry (rank uint))
  (map-get? global-leaderboard { rank: rank })
)

(define-read-only (get-player-leaderboard-position (player principal))
  (map-get? player-leaderboard-position { player: player })
)

(define-read-only (get-leaderboard-size)
  (var-get global-leaderboard-size)
)

(define-read-only (get-top-players (limit uint))
  (let ((actual-limit (if (> limit u10) u10 limit)))
    (list 
      (if (>= actual-limit u1) (map-get? global-leaderboard { rank: u1 }) none)
      (if (>= actual-limit u2) (map-get? global-leaderboard { rank: u2 }) none)
      (if (>= actual-limit u3) (map-get? global-leaderboard { rank: u3 }) none)
      (if (>= actual-limit u4) (map-get? global-leaderboard { rank: u4 }) none)
      (if (>= actual-limit u5) (map-get? global-leaderboard { rank: u5 }) none)
      (if (>= actual-limit u6) (map-get? global-leaderboard { rank: u6 }) none)
      (if (>= actual-limit u7) (map-get? global-leaderboard { rank: u7 }) none)
      (if (>= actual-limit u8) (map-get? global-leaderboard { rank: u8 }) none)
      (if (>= actual-limit u9) (map-get? global-leaderboard { rank: u9 }) none)
      (if (>= actual-limit u10) (map-get? global-leaderboard { rank: u10 }) none)
    )
  )
)

(define-public (update-leaderboard-after-tournament (tournament-id uint) (player principal) (final-position uint) (prize-amount uint))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id))
    (current-player-position (map-get? player-leaderboard-position { player: player }))
    (current-stats (get-player-stats player))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get tournament-ended tournament) err-tournament-not-ended)
    
    (let (
      (rating-change (calculate-rating-change final-position (get participant-count tournament) prize-amount))
      (current-rating (match current-player-position
        position-data (get rating position-data)
        u1000))
      (new-rating (+ current-rating rating-change))
      (new-win-rate (calculate-win-rate (get tournaments-won current-stats) (get tournaments-played current-stats)))
    )
      (match current-player-position
        existing-position (update-existing-leaderboard-position player existing-position new-rating new-win-rate prize-amount)
        (create-new-leaderboard-position player new-rating new-win-rate prize-amount)
      )
      (ok true)
    )
  )
)

(define-private (calculate-rating-change (position uint) (total-participants uint) (prize-amount uint))
  (let (
    (position-bonus (if (<= position u3) (- u4 position) u0))
    (participation-bonus u10)
    (prize-bonus (/ prize-amount u100))
  )
    (+ position-bonus participation-bonus prize-bonus)
  )
)

(define-private (calculate-win-rate (tournaments-won uint) (tournaments-played uint))
  (if (is-eq tournaments-played u0)
    u0
    (/ (* tournaments-won u100) tournaments-played)
  )
)

(define-private (update-existing-leaderboard-position (player principal) (existing-position { current-rank: uint, previous-rank: uint, rating: uint, last-tournament-date: uint }) (new-rating uint) (new-win-rate uint) (prize-amount uint))
  (let (
    (current-rank (get current-rank existing-position))
    (current-stats (get-player-stats player))
  )
    (map-set player-leaderboard-position
      { player: player }
      {
        current-rank: current-rank,
        previous-rank: current-rank,
        rating: new-rating,
        last-tournament-date: stacks-block-height
      }
    )
    
    (map-set global-leaderboard
      { rank: current-rank }
      {
        player: player,
        rating: new-rating,
        tournaments-completed: (get tournaments-played current-stats),
        total-prize-money: (get total-earnings current-stats),
        win-rate: new-win-rate
      }
    )
    true
  )
)

(define-private (create-new-leaderboard-position (player principal) (new-rating uint) (new-win-rate uint) (prize-amount uint))
  (let (
    (new-rank (+ (var-get global-leaderboard-size) u1))
    (current-stats (get-player-stats player))
  )
    (var-set global-leaderboard-size new-rank)
    
    (map-set player-leaderboard-position
      { player: player }
      {
        current-rank: new-rank,
        previous-rank: u0,
        rating: new-rating,
        last-tournament-date: stacks-block-height
      }
    )
    
    (map-set global-leaderboard
      { rank: new-rank }
      {
        player: player,
        rating: new-rating,
        tournaments-completed: (get tournaments-played current-stats),
        total-prize-money: (get total-earnings current-stats),
        win-rate: new-win-rate
      }
    )
    true
  )
)

(define-public (recompute-leaderboard-rankings)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((leaderboard-size (var-get global-leaderboard-size)))
      (try! (fold recompute-ranking-fold
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
        (ok leaderboard-size)
      ))
      (ok true)
    )
  )
)

(define-private (recompute-ranking-fold (index uint) (previous-result (response uint uint)))
  (match previous-result
    leaderboard-size (if (<= index leaderboard-size)
      (let ((entry (map-get? global-leaderboard { rank: index })))
        (match entry
          leaderboard-entry (begin
            (map-set player-leaderboard-position
              { player: (get player leaderboard-entry) }
              {
                current-rank: index,
                previous-rank: (match (map-get? player-leaderboard-position { player: (get player leaderboard-entry) })
                  existing-pos (get current-rank existing-pos)
                  u0),
                rating: (get rating leaderboard-entry),
                last-tournament-date: stacks-block-height
              }
            )
            (ok leaderboard-size)
          )
          (ok leaderboard-size)
        )
      )
      (ok leaderboard-size)
    )
    error-code (err error-code)
  )
)

;; Tournament Match Analytics System
(define-constant err-invalid-match-data (err u119))
(define-constant err-match-not-found (err u120))
(define-constant err-unauthorized-reporter (err u121))
(define-constant err-invalid-performance-score (err u122))

(define-data-var match-analytics-counter uint u0)

;; Stores detailed match performance data
(define-map match-analytics
  { analytics-id: uint }
  {
    tournament-id: uint,
    match-id: uint,
    player: principal,
    kills: uint,
    deaths: uint,
    assists: uint,
    damage-dealt: uint,
    healing-done: uint,
    objective-score: uint,
    match-duration: uint,
    performance-rating: uint
  }
)

;; Aggregated player performance statistics
(define-map player-performance-stats
  { player: principal }
  {
    total-matches: uint,
    average-kills: uint,
    average-deaths: uint,
    average-assists: uint,
    kill-death-ratio: uint,
    total-damage: uint,
    average-performance-rating: uint,
    best-performance-rating: uint,
    consistency-score: uint
  }
)

;; Head-to-head statistics between players
(define-map head-to-head-stats
  { player1: principal, player2: principal }
  {
    matches-played: uint,
    player1-wins: uint,
    player2-wins: uint,
    average-score-difference: uint
  }
)

;; Tournament-specific analytics
(define-map tournament-analytics
  { tournament-id: uint }
  {
    total-matches-recorded: uint,
    average-match-duration: uint,
    highest-performance-rating: uint,
    most-competitive-match: uint,
    total-kills: uint,
    total-damage: uint
  }
)

;; Performance trend tracking
(define-map player-performance-trends
  { player: principal, period: uint }
  {
    matches-in-period: uint,
    performance-improvement: int,
    skill-trajectory: uint,
    last-updated: uint
  }
)

;; Read-only functions for analytics queries
(define-read-only (get-match-analytics (analytics-id uint))
  (map-get? match-analytics { analytics-id: analytics-id })
)

(define-read-only (get-player-performance-stats (player principal))
  (default-to
    {
      total-matches: u0,
      average-kills: u0,
      average-deaths: u0,
      average-assists: u0,
      kill-death-ratio: u0,
      total-damage: u0,
      average-performance-rating: u0,
      best-performance-rating: u0,
      consistency-score: u0
    }
    (map-get? player-performance-stats { player: player })
  )
)

(define-read-only (get-head-to-head-stats (player1 principal) (player2 principal))
  (map-get? head-to-head-stats { player1: player1, player2: player2 })
)

(define-read-only (get-tournament-analytics (tournament-id uint))
  (map-get? tournament-analytics { tournament-id: tournament-id })
)

(define-read-only (get-player-performance-trend (player principal) (period uint))
  (map-get? player-performance-trends { player: player, period: period })
)

(define-read-only (get-analytics-counter)
  (var-get match-analytics-counter)
)

;; Calculate performance rating based on match statistics
(define-private (calculate-performance-rating (kills uint) (deaths uint) (assists uint) (damage uint) (objective uint))
  (let (
    (kd-component (if (is-eq deaths u0) (* kills u100) (/ (* kills u100) deaths)))
    (damage-component (/ damage u1000))
    (objective-component (* objective u50))
    (assist-component (* assists u25))
  )
    (+ kd-component damage-component objective-component assist-component)
  )
)

;; Record match performance data
(define-public (record-match-performance 
  (tournament-id uint) 
  (match-id uint) 
  (player principal) 
  (kills uint) 
  (deaths uint) 
  (assists uint) 
  (damage-dealt uint) 
  (healing-done uint) 
  (objective-score uint) 
  (match-duration uint))
  (let (
    (analytics-id (+ (var-get match-analytics-counter) u1))
    (performance-rating (calculate-performance-rating kills deaths assists damage-dealt objective-score))
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) err-invalid-tournament-id))
  )
    ;; Only tournament owner or authorized reporters can record data
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized-reporter)
    (asserts! (get tournament-ended tournament) err-tournament-not-ended)
    (asserts! (<= performance-rating u10000) err-invalid-performance-score)
    
    ;; Store match analytics
    (map-set match-analytics
      { analytics-id: analytics-id }
      {
        tournament-id: tournament-id,
        match-id: match-id,
        player: player,
        kills: kills,
        deaths: deaths,
        assists: assists,
        damage-dealt: damage-dealt,
        healing-done: healing-done,
        objective-score: objective-score,
        match-duration: match-duration,
        performance-rating: performance-rating
      }
    )
    
    ;; Update analytics counter
    (var-set match-analytics-counter analytics-id)
    
    ;; Update player performance statistics
    (update-player-performance-stats player kills deaths assists damage-dealt performance-rating)
    
    ;; Update tournament analytics
    (update-tournament-analytics tournament-id match-duration performance-rating kills damage-dealt)
    
    (ok analytics-id)
  )
)

;; Update aggregated player performance statistics
(define-private (update-player-performance-stats (player principal) (kills uint) (deaths uint) (assists uint) (damage uint) (performance-rating uint))
  (let (
    (current-stats (get-player-performance-stats player))
    (total-matches (+ (get total-matches current-stats) u1))
    (new-average-kills (/ (+ (* (get average-kills current-stats) (get total-matches current-stats)) kills) total-matches))
    (new-average-deaths (/ (+ (* (get average-deaths current-stats) (get total-matches current-stats)) deaths) total-matches))
    (new-average-assists (/ (+ (* (get average-assists current-stats) (get total-matches current-stats)) assists) total-matches))
    (new-total-damage (+ (get total-damage current-stats) damage))
    (new-average-performance (/ (+ (* (get average-performance-rating current-stats) (get total-matches current-stats)) performance-rating) total-matches))
    (new-best-performance (if (> performance-rating (get best-performance-rating current-stats)) performance-rating (get best-performance-rating current-stats)))
    (new-kd-ratio (if (is-eq new-average-deaths u0) (* new-average-kills u100) (/ (* new-average-kills u100) new-average-deaths)))
    (consistency-variance (calculate-consistency-score performance-rating (get average-performance-rating current-stats)))
  )
    (map-set player-performance-stats
      { player: player }
      {
        total-matches: total-matches,
        average-kills: new-average-kills,
        average-deaths: new-average-deaths,
        average-assists: new-average-assists,
        kill-death-ratio: new-kd-ratio,
        total-damage: new-total-damage,
        average-performance-rating: new-average-performance,
        best-performance-rating: new-best-performance,
        consistency-score: consistency-variance
      }
    )
    true
  )
)

;; Calculate consistency score based on performance variance
(define-private (calculate-consistency-score (current-rating uint) (average-rating uint))
  (let (
    (difference (if (> current-rating average-rating) 
                    (- current-rating average-rating) 
                    (- average-rating current-rating)))
    (variance-percentage (if (is-eq average-rating u0) u0 (/ (* difference u100) average-rating)))
  )
    (if (<= variance-percentage u10) u100
      (if (<= variance-percentage u20) u80
        (if (<= variance-percentage u30) u60
          (if (<= variance-percentage u50) u40 u20))))
  )
)

;; Update tournament-wide analytics
(define-private (update-tournament-analytics (tournament-id uint) (match-duration uint) (performance-rating uint) (kills uint) (damage uint))
  (let (
    (current-analytics (default-to
      {
        total-matches-recorded: u0,
        average-match-duration: u0,
        highest-performance-rating: u0,
        most-competitive-match: u0,
        total-kills: u0,
        total-damage: u0
      }
      (map-get? tournament-analytics { tournament-id: tournament-id })))
    (total-matches (+ (get total-matches-recorded current-analytics) u1))
    (new-average-duration (/ (+ (* (get average-match-duration current-analytics) (get total-matches-recorded current-analytics)) match-duration) total-matches))
    (new-highest-rating (if (> performance-rating (get highest-performance-rating current-analytics)) performance-rating (get highest-performance-rating current-analytics)))
  )
    (map-set tournament-analytics
      { tournament-id: tournament-id }
      {
        total-matches-recorded: total-matches,
        average-match-duration: new-average-duration,
        highest-performance-rating: new-highest-rating,
        most-competitive-match: (get most-competitive-match current-analytics),
        total-kills: (+ (get total-kills current-analytics) kills),
        total-damage: (+ (get total-damage current-analytics) damage)
      }
    )
    true
  )
)

;; Record head-to-head match result
(define-public (record-head-to-head-result (player1 principal) (player2 principal) (winner principal) (score-difference uint))
  (let (
    (current-h2h (default-to
      {
        matches-played: u0,
        player1-wins: u0,
        player2-wins: u0,
        average-score-difference: u0
      }
      (map-get? head-to-head-stats { player1: player1, player2: player2 })))
    (matches-played (+ (get matches-played current-h2h) u1))
    (player1-wins (if (is-eq winner player1) (+ (get player1-wins current-h2h) u1) (get player1-wins current-h2h)))
    (player2-wins (if (is-eq winner player2) (+ (get player2-wins current-h2h) u1) (get player2-wins current-h2h)))
    (new-avg-score-diff (/ (+ (* (get average-score-difference current-h2h) (get matches-played current-h2h)) score-difference) matches-played))
  )
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized-reporter)
    (asserts! (or (is-eq winner player1) (is-eq winner player2)) err-invalid-match-data)
    
    (map-set head-to-head-stats
      { player1: player1, player2: player2 }
      {
        matches-played: matches-played,
        player1-wins: player1-wins,
        player2-wins: player2-wins,
        average-score-difference: new-avg-score-diff
      }
    )
    (ok true)
  )
)

;; Update player performance trends
(define-public (update-performance-trend (player principal) (period uint))
  (let (
    (current-stats (get-player-performance-stats player))
    (previous-trend (map-get? player-performance-trends { player: player, period: (- period u1) }))
    (current-avg-performance (get average-performance-rating current-stats))
  )
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized-reporter)
    
    (let (
      (performance-change (match previous-trend
        prev-data (- (to-int current-avg-performance) (to-int (get skill-trajectory prev-data)))
        (to-int current-avg-performance)))
      (skill-trajectory current-avg-performance)
    )
      (map-set player-performance-trends
        { player: player, period: period }
        {
          matches-in-period: (get total-matches current-stats),
          performance-improvement: performance-change,
          skill-trajectory: skill-trajectory,
          last-updated: stacks-block-height
        }
      )
      (ok true)
    )
  )
)

;; Get top performers in a specific category
(define-read-only (get-top-performers-by-metric (metric (string-ascii 20)) (limit uint))
  (if (is-eq metric "kills")
    (get-top-killers limit)
    (if (is-eq metric "damage")
      (get-top-damage-dealers limit)
      (if (is-eq metric "rating")
        (get-top-rated-players limit)
        (list none none none none none)))))

(define-private (get-top-killers (limit uint))
  (list 
    (get-performance-rank-entry u1 "kills")
    (get-performance-rank-entry u2 "kills")
    (get-performance-rank-entry u3 "kills")
    (get-performance-rank-entry u4 "kills")
    (get-performance-rank-entry u5 "kills")
  )
)

(define-private (get-top-damage-dealers (limit uint))
  (list 
    (get-performance-rank-entry u1 "damage")
    (get-performance-rank-entry u2 "damage")
    (get-performance-rank-entry u3 "damage")
    (get-performance-rank-entry u4 "damage")
    (get-performance-rank-entry u5 "damage")
  )
)

(define-private (get-top-rated-players (limit uint))
  (list 
    (get-performance-rank-entry u1 "rating")
    (get-performance-rank-entry u2 "rating")
    (get-performance-rank-entry u3 "rating")
    (get-performance-rank-entry u4 "rating")
    (get-performance-rank-entry u5 "rating")
  )
)

(define-private (get-performance-rank-entry (rank uint) (metric (string-ascii 20)))
  none
)

