
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