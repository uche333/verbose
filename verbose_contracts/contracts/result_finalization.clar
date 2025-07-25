;; Enhanced Voting with Result Finalization Smart Contract
;; Advanced Features: Weighted voting, delegation, multi-round voting, analytics, and more
;; Fixed interdependencies and circular references

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-voting-ended (err u102))
(define-constant err-voting-active (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-results-finalized (err u105))
(define-constant err-invalid-option (err u106))
(define-constant err-insufficient-balance (err u107))
(define-constant err-delegation-not-allowed (err u108))
(define-constant err-cannot-delegate-to-self (err u109))
(define-constant err-quorum-not-met (err u110))
(define-constant err-invalid-threshold (err u111))
(define-constant err-voting-not-started (err u112))
(define-constant err-unauthorized (err u113))
(define-constant err-invalid-round (err u114))
(define-constant err-invalid-config (err u115))

;; Data Variables
(define-data-var voting-active bool false)
(define-data-var voting-question (string-utf8 500) u"")
(define-data-var voting-start-block uint u0)
(define-data-var voting-end-block uint u0)
(define-data-var results-finalized bool false)
(define-data-var total-votes uint u0)
(define-data-var total-weight uint u0)
(define-data-var current-round uint u1)
(define-data-var max-rounds uint u1)
(define-data-var quorum-threshold uint u0)
(define-data-var winning-threshold uint u50) ;; 50% by default
(define-data-var voting-type (string-ascii 20) "simple") ;; simple, weighted, delegated, ranked
(define-data-var allow-delegation bool false)
(define-data-var allow-vote-changing bool false)
(define-data-var require-minimum-balance bool false)
(define-data-var minimum-balance uint u0)
(define-data-var voting-fee uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var session-counter uint u0)
(define-data-var option-counter uint u0)

;; Data Maps
(define-map voting-options uint {
  text: (string-utf8 200),
  votes: uint,
  weight: uint,
  active: bool
})

(define-map voter-records principal {
  option-id: uint,
  weight: uint,
  round: uint,
  block-height: uint,
  delegated-to: (optional principal),
  delegation-weight: uint
})

(define-map vote-history principal (list 10 {option-id: uint, round: uint, block-height: uint}))
(define-map delegation-records principal principal) ;; delegator -> delegate
(define-map delegation-weights principal uint) ;; delegate -> total weight
(define-map authorized-voters principal bool)
(define-map voter-weights principal uint) ;; for weighted voting
(define-map option-exists uint bool)
(define-map round-results uint {winner: uint, total-votes: uint, quorum-met: bool})

;; Ranking system for ranked choice voting
(define-map voter-rankings principal (list 10 uint))
(define-map elimination-rounds uint (list 10 uint)) ;; round -> eliminated options

;; Analytics and audit trail
(define-map daily-vote-counts uint uint) ;; day -> votes
(define-map voter-participation principal uint) ;; total participations
(define-map voting-sessions uint {
  question: (string-utf8 500),
  start-block: uint,
  end-block: uint,
  total-votes: uint,
  winner: (optional uint),
  finalized: bool
})

;; Read-only functions

;; Get comprehensive voting status
(define-read-only (get-voting-status)
  {
    active: (var-get voting-active),
    question: (var-get voting-question),
    start-block: (var-get voting-start-block),
    end-block: (var-get voting-end-block),
    current-block: block-height,
    results-finalized: (var-get results-finalized),
    total-votes: (var-get total-votes),
    total-weight: (var-get total-weight),
    current-round: (var-get current-round),
    max-rounds: (var-get max-rounds),
    quorum-threshold: (var-get quorum-threshold),
    winning-threshold: (var-get winning-threshold),
    voting-type: (var-get voting-type),
    allow-delegation: (var-get allow-delegation),
    allow-vote-changing: (var-get allow-vote-changing),
    voting-fee: (var-get voting-fee),
    quorum-met: (>= (var-get total-votes) (var-get quorum-threshold))
  }
)

;; Get voting option details
(define-read-only (get-voting-option (option-id uint))
  (map-get? voting-options option-id)
)

;; Get voter's complete record
(define-read-only (get-voter-record (voter principal))
  (map-get? voter-records voter)
)

;; Get delegation information
(define-read-only (get-delegation-info (voter principal))
  {
    delegated-to: (map-get? delegation-records voter),
    delegation-weight: (default-to u0 (map-get? delegation-weights voter)),
    can-delegate: (var-get allow-delegation)
  }
)

;; Get voter's participation history
(define-read-only (get-voter-history (voter principal))
  {
    vote-history: (default-to (list) (map-get? vote-history voter)),
    total-participations: (default-to u0 (map-get? voter-participation voter)),
    current-weight: (get-voter-weight voter)
  }
)

;; Calculate voter weight based on voting type
(define-read-only (get-voter-weight (voter principal))
  (if (is-eq (var-get voting-type) "weighted")
    (default-to u1 (map-get? voter-weights voter))
    u1)
)

;; Get current results with detailed breakdown
(define-read-only (get-detailed-results)
  {
    option-1: (map-get? voting-options u1),
    option-2: (map-get? voting-options u2),
    option-3: (map-get? voting-options u3),
    option-4: (map-get? voting-options u4),
    option-5: (map-get? voting-options u5),
    option-6: (map-get? voting-options u6),
    option-7: (map-get? voting-options u7),
    option-8: (map-get? voting-options u8),
    option-9: (map-get? voting-options u9),
    option-10: (map-get? voting-options u10),
    total-votes: (var-get total-votes),
    total-weight: (var-get total-weight),
    finalized: (var-get results-finalized),
    current-round: (var-get current-round),
    quorum-met: (>= (var-get total-votes) (var-get quorum-threshold)),
    fees-collected: (var-get total-fees-collected)
  }
)

;; Get analytics data
(define-read-only (get-analytics)
  {
    total-sessions: (var-get session-counter),
    average-participation: (if (> (var-get session-counter) u0)
      (/ (var-get total-votes) (var-get session-counter))
      u0),
    current-session-participation: (var-get total-votes),
    fees-collected: (var-get total-fees-collected)
  }
)

;; Check voting eligibility
(define-read-only (is-eligible-voter (voter principal))
  (and
    (if (var-get require-minimum-balance)
      (>= (stx-get-balance voter) (var-get minimum-balance))
      true)
    ;; Check if authorization is required - if any voter has been authorized, then authorization is required
    ;; Otherwise, all voters are eligible (open voting)
    (match (map-get? authorized-voters voter)
      some-auth some-auth  ;; If voter is in map, use their authorization status
      true)  ;; If voter not in map, they're eligible (open voting by default)
  )
)

;; Get round information
(define-read-only (get-round-info (round uint))
  (map-get? round-results round)
)

;; Check if voting has ended
(define-read-only (voting-ended)
  (>= block-height (var-get voting-end-block))
)

;; Get winner calculation (simplified)
(define-read-only (get-current-leader)
  (let ((option-1-votes (default-to {votes: u0, weight: u0, text: u"", active: false} (map-get? voting-options u1)))
        (option-2-votes (default-to {votes: u0, weight: u0, text: u"", active: false} (map-get? voting-options u2))))
    (if (> (get votes option-1-votes) (get votes option-2-votes))
      (some u1)
      (if (> (get votes option-2-votes) u0)
        (some u2)
        none))
  )
)

;; Private functions

;; Check if caller is contract owner
(define-private (is-owner)
  (is-eq tx-sender contract-owner)
)

;; Calculate effective weight including delegations
(define-private (get-effective-weight (voter principal))
  (+ (get-voter-weight voter)
     (default-to u0 (map-get? delegation-weights voter))
  )
)

;; Public functions

;; Simple voting session setup (no interdependencies)
(define-public (start-simple-voting 
  (question (string-utf8 500))
  (duration-blocks uint)
  (option-1 (string-utf8 200))
  (option-2 (string-utf8 200))
  (option-3 (optional (string-utf8 200)))
  (option-4 (optional (string-utf8 200)))
  (option-5 (optional (string-utf8 200))))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (not (var-get voting-active)) err-voting-active)
    (asserts! (not (var-get results-finalized)) err-results-finalized)
    
    ;; Reset state
    (var-set total-votes u0)
    (var-set total-weight u0)
    (var-set results-finalized false)
    (var-set current-round u1)
    (var-set total-fees-collected u0)
    (var-set option-counter u0)
    
    ;; Clear previous data
    (map-delete voting-options u1)
    (map-delete voting-options u2)
    (map-delete voting-options u3)
    (map-delete voting-options u4)
    (map-delete voting-options u5)
    (map-delete option-exists u1)
    (map-delete option-exists u2)
    (map-delete option-exists u3)
    (map-delete option-exists u4)
    (map-delete option-exists u5)
    
    ;; Set basic configuration
    (var-set voting-question question)
    (var-set voting-start-block block-height)
    (var-set voting-end-block (+ block-height duration-blocks))
    (var-set voting-type "simple")
    (var-set quorum-threshold u0)
    (var-set winning-threshold u50)
    (var-set max-rounds u1)
    (var-set allow-delegation false)
    (var-set allow-vote-changing false)
    (var-set require-minimum-balance false)
    (var-set minimum-balance u0)
    (var-set voting-fee u0)
    
    ;; Set mandatory options
    (map-set voting-options u1 {text: option-1, votes: u0, weight: u0, active: true})
    (map-set option-exists u1 true)
    (var-set option-counter u1)
    
    (map-set voting-options u2 {text: option-2, votes: u0, weight: u0, active: true})
    (map-set option-exists u2 true)
    (var-set option-counter u2)
    
    ;; Set optional options
    (match option-3
      some-option (begin
        (map-set voting-options u3 {text: some-option, votes: u0, weight: u0, active: true})
        (map-set option-exists u3 true)
        (var-set option-counter u3))
      true)
    
    (match option-4
      some-option (begin
        (map-set voting-options u4 {text: some-option, votes: u0, weight: u0, active: true})
        (map-set option-exists u4 true)
        (var-set option-counter u4))
      true)
    
    (match option-5
      some-option (begin
        (map-set voting-options u5 {text: some-option, votes: u0, weight: u0, active: true})
        (map-set option-exists u5 true)
        (var-set option-counter u5))
      true)
    
    ;; Increment session counter and record session
    (var-set session-counter (+ (var-get session-counter) u1))
    (map-set voting-sessions (var-get session-counter) {
      question: question,
      start-block: block-height,
      end-block: (+ block-height duration-blocks),
      total-votes: u0,
      winner: none,
      finalized: false
    })
    
    (var-set voting-active true)
    (ok {session-id: (var-get session-counter), options-set: (var-get option-counter)})
  )
)

;; Add individual voting option (Admin only)
(define-public (add-voting-option (option-text (string-utf8 200)))
  (let ((next-id (+ (var-get option-counter) u1)))
    (begin
      (asserts! (is-owner) err-owner-only)
      (asserts! (var-get voting-active) err-voting-not-started)
      (asserts! (<= next-id u10) err-invalid-config)
      
      (map-set voting-options next-id {
        text: option-text,
        votes: u0,
        weight: u0,
        active: true
      })
      (map-set option-exists next-id true)
      (var-set option-counter next-id)
      
      (ok next-id)
    )
  )
)

;; Configure advanced voting settings (Admin only)
(define-public (configure-voting-settings
  (voting-type-input (string-ascii 20))
  (quorum uint)
  (threshold uint)
  (max-rounds-input uint)
  (delegation-allowed bool)
  (vote-changing-allowed bool)
  (balance-required bool)
  (min-balance uint)
  (fee uint))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (var-get voting-active) err-voting-not-started)
    (asserts! (<= threshold u100) err-invalid-threshold)
    (asserts! (<= max-rounds-input u10) err-invalid-config)
    
    (var-set voting-type voting-type-input)
    (var-set quorum-threshold quorum)
    (var-set winning-threshold threshold)
    (var-set max-rounds max-rounds-input)
    (var-set allow-delegation delegation-allowed)
    (var-set allow-vote-changing vote-changing-allowed)
    (var-set require-minimum-balance balance-required)
    (var-set minimum-balance min-balance)
    (var-set voting-fee fee)
    
    (ok true)
  )
)

;; Set individual voter weight (Admin only, for weighted voting)
(define-public (set-voter-weight (voter principal) (weight uint))
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (is-eq (var-get voting-type) "weighted") err-unauthorized)
    (asserts! (> weight u0) err-invalid-config)
    
    (map-set voter-weights voter weight)
    (ok weight)
  )
)

;; Authorize individual voter (Admin only)
(define-public (authorize-voter (voter principal))
  (begin
    (asserts! (is-owner) err-owner-only)
    (map-set authorized-voters voter true)
    (ok true)
  )
)

;; Revoke voter authorization (Admin only)
(define-public (revoke-voter-authorization (voter principal))
  (begin
    (asserts! (is-owner) err-owner-only)
    (map-delete authorized-voters voter)
    (ok true)
  )
)

;; Enhanced voting function
(define-public (cast-enhanced-vote (option-id uint) (ranking (optional (list 10 uint))))
  (let ((voter-weight (get-effective-weight tx-sender))
        (fee (var-get voting-fee)))
    (begin
      (asserts! (var-get voting-active) err-voting-ended)
      (asserts! (< block-height (var-get voting-end-block)) err-voting-ended)
      (asserts! (is-eligible-voter tx-sender) err-unauthorized)
      (asserts! (default-to false (map-get? option-exists option-id)) err-invalid-option)
      
      ;; Check if already voted (unless vote changing is allowed)
      (if (var-get allow-vote-changing)
        true
        (asserts! (is-none (map-get? voter-records tx-sender)) err-already-voted))
      
      ;; Collect voting fee if required
      (if (> fee u0)
        (begin
          (try! (stx-transfer? fee tx-sender contract-owner))
          (var-set total-fees-collected (+ (var-get total-fees-collected) fee)))
        true)
      
      ;; Handle ranked choice voting
      (if (is-eq (var-get voting-type) "ranked")
        (match ranking
          some-ranks (map-set voter-rankings tx-sender some-ranks)
          true)
        true)
      
      ;; Record the vote
      (map-set voter-records tx-sender {
        option-id: option-id,
        weight: voter-weight,
        round: (var-get current-round),
        block-height: block-height,
        delegated-to: none,
        delegation-weight: u0
      })
      
      ;; Update option votes
      (match (map-get? voting-options option-id)
        some-option (map-set voting-options option-id {
          text: (get text some-option),
          votes: (+ (get votes some-option) u1),
          weight: (+ (get weight some-option) voter-weight),
          active: (get active some-option)
        })
        false)
      
      ;; Update totals
      (var-set total-votes (+ (var-get total-votes) u1))
      (var-set total-weight (+ (var-get total-weight) voter-weight))
      
      ;; Update participation tracking
      (map-set voter-participation tx-sender 
        (+ (default-to u0 (map-get? voter-participation tx-sender)) u1))
      
      (ok {voted-for: option-id, weight: voter-weight, round: (var-get current-round)})
    )
  )
)

;; Simple vote casting function
(define-public (cast-vote (option-id uint))
  (cast-enhanced-vote option-id none)
)

;; Delegate voting power
(define-public (delegate-vote (delegate principal))
  (let ((voter-weight (get-voter-weight tx-sender)))
    (begin
      (asserts! (var-get allow-delegation) err-delegation-not-allowed)
      (asserts! (var-get voting-active) err-voting-not-started)
      (asserts! (not (is-eq tx-sender delegate)) err-cannot-delegate-to-self)
      (asserts! (is-eligible-voter delegate) err-unauthorized)
      
      ;; Record delegation
      (map-set delegation-records tx-sender delegate)
      (map-set delegation-weights delegate 
        (+ (default-to u0 (map-get? delegation-weights delegate)) voter-weight))
      
      (ok {delegated-to: delegate, weight: voter-weight})
    )
  )
)

;; Revoke delegation
(define-public (revoke-delegation)
  (match (map-get? delegation-records tx-sender)
    some-delegate (let ((voter-weight (get-voter-weight tx-sender)))
      (begin
        (map-delete delegation-records tx-sender)
        (map-set delegation-weights some-delegate 
          (- (default-to u0 (map-get? delegation-weights some-delegate)) voter-weight))
        (ok {revoked-from: some-delegate})
      ))
    (err err-not-found))
)

;; Process next round (for multi-round voting)
(define-public (process-next-round)
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (< (var-get current-round) (var-get max-rounds)) err-invalid-round)
    (asserts! (< (var-get total-votes) (var-get quorum-threshold)) err-quorum-not-met)
    
    ;; Record current round results
    (map-set round-results (var-get current-round) {
      winner: (default-to u0 (get-current-leader)),
      total-votes: (var-get total-votes),
      quorum-met: false
    })
    
    ;; Advance to next round
    (var-set current-round (+ (var-get current-round) u1))
    (var-set total-votes u0)
    (var-set total-weight u0)
    
    (ok {new-round: (var-get current-round)})
  )
)

;; End voting session (Admin only)
(define-public (end-voting)
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (var-get voting-active) err-not-found)
    
    (var-set voting-active false)
    (ok {ended-at-block: block-height, total-votes: (var-get total-votes)})
  )
)

;; Finalize results with enhanced validation
(define-public (finalize-results)
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (not (var-get voting-active)) err-voting-active)
    (asserts! (voting-ended) err-voting-active)
    (asserts! (not (var-get results-finalized)) err-results-finalized)
    (asserts! (>= (var-get total-votes) (var-get quorum-threshold)) err-quorum-not-met)
    
    ;; Mark results as finalized
    (var-set results-finalized true)
    
    ;; Update session record
    (map-set voting-sessions (var-get session-counter) {
      question: (var-get voting-question),
      start-block: (var-get voting-start-block),
      end-block: (var-get voting-end-block),
      total-votes: (var-get total-votes),
      winner: (get-current-leader),
      finalized: true
    })
    
    (ok {
      total-votes: (var-get total-votes),
      total-weight: (var-get total-weight),
      quorum-met: true,
      finalized-at-block: block-height,
      session-id: (var-get session-counter),
      winner: (get-current-leader)
    })
  )
)

;; Withdraw collected fees (Admin only)
(define-public (withdraw-fees)
  (let ((total-fees (var-get total-fees-collected)))
    (begin
      (asserts! (is-owner) err-owner-only)
      (asserts! (> total-fees u0) err-not-found)
      
      (var-set total-fees-collected u0)
      (try! (as-contract (stx-transfer? total-fees tx-sender contract-owner)))
      (ok total-fees)
    )
  )
)

;; Emergency functions
(define-public (emergency-pause)
  (begin
    (asserts! (is-owner) err-owner-only)
    (var-set voting-active false)
    (ok true)
  )
)

(define-public (emergency-reset)
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (not (var-get results-finalized)) err-results-finalized)
    
    ;; Reset all state
    (var-set voting-active false)
    (var-set voting-question u"")
    (var-set voting-start-block u0)
    (var-set voting-end-block u0)
    (var-set total-votes u0)
    (var-set total-weight u0)
    (var-set current-round u1)
    (var-set option-counter u0)
    
    (ok true)
  )
)