;; Team Formation System - Team Creation and Management for Esports Tournaments
;; Enables players to form teams, manage rosters, and participate in team-based tournaments

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_TEAM_NOT_FOUND (err u301))
(define-constant ERR_TEAM_FULL (err u302))
(define-constant ERR_ALREADY_ON_TEAM (err u303))
(define-constant ERR_NOT_TEAM_MEMBER (err u304))
(define-constant ERR_NOT_TEAM_CAPTAIN (err u305))
(define-constant ERR_INVALID_TEAM_SIZE (err u306))
(define-constant ERR_TEAM_NAME_EXISTS (err u307))
(define-constant ERR_INVITATION_NOT_FOUND (err u308))
(define-constant ERR_INVITATION_EXPIRED (err u309))
(define-constant ERR_CANNOT_INVITE_SELF (err u310))

;; Data variables
(define-data-var team-counter uint u0)
(define-data-var invitation-counter uint u0)

;; Team data structure
(define-map teams
  uint
  {
    name: (string-ascii 50),
    captain: principal,
    description: (string-ascii 200),
    max-size: uint,
    current-size: uint,
    created-at: uint,
    is-active: bool,
    wins: uint,
    losses: uint,
    total-prize-money: uint
  }
)

;; Team membership tracking
(define-map team-members
  { team-id: uint, member: principal }
  {
    joined-at: uint,
    role: (string-ascii 20),
    is-active: bool
  }
)

;; Player team association - one player can only be on one team at a time
(define-map player-teams
  principal
  {
    current-team-id: uint,
    joined-at: uint,
    role: (string-ascii 20)
  }
)

;; Team invitations system
(define-map team-invitations
  uint
  {
    team-id: uint,
    invited-player: principal,
    invited-by: principal,
    expires-at: uint,
    message: (string-ascii 100),
    is-accepted: bool,
    is-active: bool
  }
)

;; Team name registry to prevent duplicates
(define-map team-names
  (string-ascii 50)
  { team-id: uint }
)

;; Team statistics and achievements
(define-map team-tournament-stats
  { team-id: uint, tournament-id: uint }
  {
    placement: uint,
    prize-earned: uint,
    matches-played: uint,
    matches-won: uint
  }
)

;; Read-only functions

(define-read-only (get-team (team-id uint))
  (map-get? teams team-id))

(define-read-only (get-team-by-name (name (string-ascii 50)))
  (match (map-get? team-names name)
    name-record (map-get? teams (get team-id name-record))
    none))

(define-read-only (get-player-team (player principal))
  (map-get? player-teams player))

(define-read-only (get-team-member-info (team-id uint) (member principal))
  (map-get? team-members { team-id: team-id, member: member }))

(define-read-only (get-team-invitation (invitation-id uint))
  (map-get? team-invitations invitation-id))

(define-read-only (get-team-counter)
  (var-get team-counter))

(define-read-only (is-team-captain (team-id uint) (player principal))
  (match (map-get? teams team-id)
    team (is-eq (get captain team) player)
    false))

;; Public functions

(define-public (create-team 
  (name (string-ascii 50)) 
  (description (string-ascii 200)) 
  (max-size uint))
  (let ((team-id (+ (var-get team-counter) u1)))
    
    ;; Validate parameters
    (asserts! (and (>= max-size u2) (<= max-size u10)) ERR_INVALID_TEAM_SIZE)
    (asserts! (is-none (map-get? team-names name)) ERR_TEAM_NAME_EXISTS)
    (asserts! (is-none (map-get? player-teams tx-sender)) ERR_ALREADY_ON_TEAM)
    
    ;; Create team
    (map-set teams team-id
      {
        name: name,
        captain: tx-sender,
        description: description,
        max-size: max-size,
        current-size: u1,
        created-at: stacks-block-height,
        is-active: true,
        wins: u0,
        losses: u0,
        total-prize-money: u0
      })
    
    ;; Add captain as first team member
    (map-set team-members
      { team-id: team-id, member: tx-sender }
      {
        joined-at: stacks-block-height,
        role: "captain",
        is-active: true
      })
    
    ;; Update player team association
    (map-set player-teams tx-sender
      {
        current-team-id: team-id,
        joined-at: stacks-block-height,
        role: "captain"
      })
    
    ;; Register team name
    (map-set team-names name { team-id: team-id })
    
    (var-set team-counter team-id)
    (ok team-id)))

(define-public (invite-to-team 
  (team-id uint) 
  (player principal) 
  (message (string-ascii 100)))
  (let ((team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
        (invitation-id (+ (var-get invitation-counter) u1)))
    
    (asserts! (is-eq tx-sender (get captain team)) ERR_NOT_TEAM_CAPTAIN)
    (asserts! (get is-active team) ERR_TEAM_NOT_FOUND)
    (asserts! (< (get current-size team) (get max-size team)) ERR_TEAM_FULL)
    (asserts! (not (is-eq tx-sender player)) ERR_CANNOT_INVITE_SELF)
    (asserts! (is-none (map-get? player-teams player)) ERR_ALREADY_ON_TEAM)
    
    ;; Create invitation (expires in 1 week)
    (map-set team-invitations invitation-id
      {
        team-id: team-id,
        invited-player: player,
        invited-by: tx-sender,
        expires-at: (+ stacks-block-height u1008), ;; ~1 week
        message: message,
        is-accepted: false,
        is-active: true
      })
    
    (var-set invitation-counter invitation-id)
    (ok invitation-id)))

(define-public (accept-team-invitation (invitation-id uint))
  (let ((invitation (unwrap! (map-get? team-invitations invitation-id) ERR_INVITATION_NOT_FOUND))
        (team (unwrap! (map-get? teams (get team-id invitation)) ERR_TEAM_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get invited-player invitation)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active invitation) ERR_INVITATION_NOT_FOUND)
    (asserts! (< stacks-block-height (get expires-at invitation)) ERR_INVITATION_EXPIRED)
    (asserts! (< (get current-size team) (get max-size team)) ERR_TEAM_FULL)
    (asserts! (is-none (map-get? player-teams tx-sender)) ERR_ALREADY_ON_TEAM)
    
    ;; Accept invitation
    (map-set team-invitations invitation-id
      (merge invitation { is-accepted: true, is-active: false }))
    
    ;; Add player to team
    (map-set team-members
      { team-id: (get team-id invitation), member: tx-sender }
      {
        joined-at: stacks-block-height,
        role: "member",
        is-active: true
      })
    
    ;; Update player team association
    (map-set player-teams tx-sender
      {
        current-team-id: (get team-id invitation),
        joined-at: stacks-block-height,
        role: "member"
      })
    
    ;; Update team size
    (map-set teams (get team-id invitation)
      (merge team { current-size: (+ (get current-size team) u1) }))
    
    (ok true)))

(define-public (leave-team (team-id uint))
  (let ((team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
        (member-info (unwrap! (map-get? team-members { team-id: team-id, member: tx-sender }) ERR_NOT_TEAM_MEMBER))
        (player-team (unwrap! (map-get? player-teams tx-sender) ERR_NOT_TEAM_MEMBER)))
    
    (asserts! (is-eq (get current-team-id player-team) team-id) ERR_NOT_TEAM_MEMBER)
    (asserts! (get is-active member-info) ERR_NOT_TEAM_MEMBER)
    
    ;; Captain cannot leave unless team is disbanded or they transfer captaincy
    (asserts! (not (is-eq tx-sender (get captain team))) ERR_NOT_AUTHORIZED)
    
    ;; Remove from team
    (map-set team-members
      { team-id: team-id, member: tx-sender }
      (merge member-info { is-active: false }))
    
    ;; Remove player team association
    (map-delete player-teams tx-sender)
    
    ;; Update team size
    (map-set teams team-id
      (merge team { current-size: (- (get current-size team) u1) }))
    
    (ok true)))

(define-public (transfer-captaincy (team-id uint) (new-captain principal))
  (let ((team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
        (new-captain-info (unwrap! (map-get? team-members { team-id: team-id, member: new-captain }) ERR_NOT_TEAM_MEMBER)))
    
    (asserts! (is-eq tx-sender (get captain team)) ERR_NOT_TEAM_CAPTAIN)
    (asserts! (get is-active new-captain-info) ERR_NOT_TEAM_MEMBER)
    
    ;; Update team captain
    (map-set teams team-id
      (merge team { captain: new-captain }))
    
    ;; Update old captain role
    (map-set team-members
      { team-id: team-id, member: tx-sender }
      {
        joined-at: (get joined-at (unwrap-panic (map-get? team-members { team-id: team-id, member: tx-sender }))),
        role: "member",
        is-active: true
      })
    
    ;; Update new captain role
    (map-set team-members
      { team-id: team-id, member: new-captain }
      (merge new-captain-info { role: "captain" }))
    
    ;; Update player team associations
    (map-set player-teams tx-sender
      (merge (unwrap-panic (map-get? player-teams tx-sender)) { role: "member" }))
    
    (map-set player-teams new-captain
      (merge (unwrap-panic (map-get? player-teams new-captain)) { role: "captain" }))
    
    (ok true)))

(define-public (disband-team (team-id uint))
  (let ((team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get captain team)) ERR_NOT_TEAM_CAPTAIN)
    
    ;; Deactivate team
    (map-set teams team-id
      (merge team { is-active: false }))
    
    (ok true)))

(define-public (update-team-stats 
  (team-id uint) 
  (tournament-id uint) 
  (placement uint) 
  (prize-earned uint) 
  (matches-played uint) 
  (matches-won uint))
  (let ((team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND)))
    
    ;; Only contract owner can update stats (called from main tournament contract)
    ;; For now, allow team captains to update their own team stats
    (asserts! (or (is-eq tx-sender (get captain team)) (is-eq tx-sender (get captain team))) ERR_NOT_AUTHORIZED)
    
    ;; Update team tournament stats
    (map-set team-tournament-stats
      { team-id: team-id, tournament-id: tournament-id }
      {
        placement: placement,
        prize-earned: prize-earned,
        matches-played: matches-played,
        matches-won: matches-won
      })
    
    ;; Update overall team stats
    (let ((wins-to-add (if (<= placement u3) u1 u0))
          (losses-to-add (if (> placement u3) u1 u0)))
      (map-set teams team-id
        (merge team {
          wins: (+ (get wins team) wins-to-add),
          losses: (+ (get losses team) losses-to-add),
          total-prize-money: (+ (get total-prize-money team) prize-earned)
        })))
    
    (ok true)))
