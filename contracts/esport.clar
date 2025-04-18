
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