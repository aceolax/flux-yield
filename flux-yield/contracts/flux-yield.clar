;; FluxYield Music Streaming Platform
;; Decentralized music streaming with tokenized rights and yield farming

;; =============================================================================
;; ERROR CONSTANTS
;; =============================================================================
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-TRACK-NOT-FOUND (err u103))
(define-constant ERR-TRACK-ALREADY-MINTED (err u104))
(define-constant ERR-CONSENSUS-NOT-REACHED (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-INVALID-STATE (err u107))
(define-constant ERR-DEADLINE-PASSED (err u108))
(define-constant ERR-TRANSFER-FAILED (err u109))

;; =============================================================================
;; CONSTANTS AND VARIABLES
;; =============================================================================
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PLATFORM-FEE-RATE u250) ;; 2.5%
(define-constant MIN-VOTE-THRESHOLD u1000)
(define-constant REWARD-POOL-RATE u50) ;; 5%
(define-constant COUNCIL-THRESHOLD u5000)

(define-data-var total-supply uint u0)
(define-data-var next-track-id uint u1)
(define-data-var yield-multiplier uint u150)
(define-data-var global-streaming-volume uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; Token balances for FluxYield tokens
(define-map token-balances principal uint)

;; Music track information
(define-map tracks
  uint
  {
    creator: principal,
    title: (string-ascii 128),
    genre: (string-ascii 32),
    funding-goal: uint,
    current-funding: uint,
    status: (string-ascii 16),
    votes: uint,
    voter-count: uint,
    is-minted: bool,
    royalty-score: uint,
    created-at: uint,
    deadline: uint
  }
)

;; Rights distribution for each track
(define-map rights-distribution
  uint
  {
    artist-share: uint,
    producer-share: uint,
    songwriter-share: uint,
    fan-share: uint,
    total-shares: uint
  }
)

;; User voting records
(define-map votes
  { voter: principal, track-id: uint }
  {
    amount: uint,
    timestamp: uint,
    weight: uint
  }
)

;; Listening verification records
(define-map listen-records
  { user: principal, track-id: uint }
  {
    listen-count: uint,
    total-value: uint,
    last-listen: uint,
    verification-score: uint
  }
)

;; Rights council members
(define-map council-members
  principal
  {
    contributions: uint,
    voting-weight: uint,
    verified-listens: uint,
    reputation-score: uint,
    is-active: bool
  }
)

;; Label partnerships
(define-map label-partners
  principal
  {
    name: (string-ascii 64),
    total-sponsored: uint,
    active-deals: uint,
    reputation: uint
  }
)

;; =============================================================================
;; PRIVATE HELPER FUNCTIONS
;; =============================================================================

(define-private (calculate-voting-weight (user principal) (base-amount uint))
  (let
    (
      (user-balance (default-to u0 (map-get? token-balances user)))
      (council-data (map-get? council-members user))
      (is-council-member (match council-data
        member (get is-active member)
        false))
      (multiplier (if is-council-member u150 u100))
    )
    (/ (* base-amount multiplier) u100)
  )
)

(define-private (calculate-listen-reward (listen-type (string-ascii 32)) (value uint))
  (let
    (
      (base-rate (var-get yield-multiplier))
      (type-multiplier (if (is-eq listen-type "premium") u200 u100))
    )
    (/ (* value base-rate type-multiplier) u10000)
  )
)

(define-private (transfer-tokens (from principal) (to principal) (amount uint))
  (let
    (
      (from-balance (default-to u0 (map-get? token-balances from)))
      (to-balance (default-to u0 (map-get? token-balances to)))
    )
    (if (>= from-balance amount)
      (begin
        (map-set token-balances from (- from-balance amount))
        (map-set token-balances to (+ to-balance amount))
        (ok true))
      ERR-INSUFFICIENT-BALANCE)
  )
)

(define-private (distribute-rewards (track-id uint))
  (match (map-get? tracks track-id)
    track (let
      (
        (reward-pool (/ (* (get funding-goal track) REWARD-POOL-RATE) u1000))
      )
      (var-set global-streaming-volume 
        (+ (var-get global-streaming-volume) (get royalty-score track)))
      reward-pool
    )
    u0
  )
)

(define-private (update-council-status (user principal) (contribution uint))
  (let
    (
      (existing (map-get? council-members user))
      (current-contributions (match existing
        member (get contributions member)
        u0))
      (new-total (+ current-contributions contribution))
      (qualifies (>= new-total COUNCIL-THRESHOLD))
    )
    (map-set council-members user
      {
        contributions: new-total,
        voting-weight: (if qualifies u150 u100),
        verified-listens: (match existing
          member (+ (get verified-listens member) u1)
          u1),
        reputation-score: (calculate-listen-reward "standard" contribution),
        is-active: qualifies
      })
    qualifies
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS
;; =============================================================================

;; Mint tokens (owner only)
(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (map-set token-balances recipient 
      (+ (default-to u0 (map-get? token-balances recipient)) amount))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok amount)
  )
)

;; Create a new music track for funding
(define-public (create-track 
  (title (string-ascii 128))
  (genre (string-ascii 32))
  (funding-goal uint)
  (deadline uint))
  (let
    (
      (track-id (var-get next-track-id))
      (creator-balance (default-to u0 (map-get? token-balances tx-sender)))
    )
    (asserts! (>= creator-balance u100) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> funding-goal u0) ERR-INVALID-AMOUNT)
    (asserts! (> deadline block-height) ERR-DEADLINE-PASSED)
    
    (map-set tracks track-id
      {
        creator: tx-sender,
        title: title,
        genre: genre,
        funding-goal: funding-goal,
        current-funding: u0,
        status: "funding",
        votes: u0,
        voter-count: u0,
        is-minted: false,
        royalty-score: u0,
        created-at: block-height,
        deadline: deadline
      })
    
    (map-set rights-distribution track-id
      {
        artist-share: u400,     ;; 40%
        producer-share: u200,   ;; 20%
        songwriter-share: u200, ;; 20%
        fan-share: u200,       ;; 20%
        total-shares: u1000
      })
    
    (var-set next-track-id (+ track-id u1))
    (ok track-id)
  )
)

;; Vote on a track with token weight
(define-public (vote-on-track (track-id uint) (vote-amount uint))
  (let
    (
      (track (unwrap! (map-get? tracks track-id) ERR-TRACK-NOT-FOUND))
      (voter-balance (default-to u0 (map-get? token-balances tx-sender)))
      (vote-weight (calculate-voting-weight tx-sender vote-amount))
      (existing-vote (map-get? votes {voter: tx-sender, track-id: track-id}))
    )
    (asserts! (not (get is-minted track)) ERR-TRACK-ALREADY-MINTED)
    (asserts! (>= voter-balance vote-amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (<= block-height (get deadline track)) ERR-DEADLINE-PASSED)
    
    ;; Record the vote
    (map-set votes {voter: tx-sender, track-id: track-id}
      {
        amount: vote-amount,
        timestamp: block-height,
        weight: vote-weight
      })
    
    ;; Update track vote totals
    (map-set tracks track-id
      (merge track
        {
          votes: (+ (get votes track) vote-weight),
          voter-count: (+ (get voter-count track) u1),
          current-funding: (+ (get current-funding track) vote-amount)
        }))
    
    ;; Transfer tokens directly
    (map-set token-balances tx-sender (- voter-balance vote-amount))
    (map-set token-balances (as-contract tx-sender) 
      (+ (default-to u0 (map-get? token-balances (as-contract tx-sender))) vote-amount))
    (ok vote-weight)
  )
)

;; Mint streaming rights NFT after successful funding
(define-public (mint-streaming-rights (track-id uint))
  (let
    (
      (track (unwrap! (map-get? tracks track-id) ERR-TRACK-NOT-FOUND))
      (reward-amount (distribute-rewards track-id))
    )
    (asserts! (not (get is-minted track)) ERR-TRACK-ALREADY-MINTED)
    (asserts! (>= (get votes track) MIN-VOTE-THRESHOLD) ERR-CONSENSUS-NOT-REACHED)
    (asserts! (>= (get current-funding track) (get funding-goal track)) ERR-CONSENSUS-NOT-REACHED)
    (asserts! (<= block-height (get deadline track)) ERR-DEADLINE-PASSED)
    
    (map-set tracks track-id
      (merge track
        {
          is-minted: true,
          status: "minted"
        }))
    
    (ok reward-amount)
  )
)

;; Submit listening verification
(define-public (verify-listen 
  (track-id uint)
  (listen-type (string-ascii 32))
  (stream-value uint))
  (let
    (
      (track (unwrap! (map-get? tracks track-id) ERR-TRACK-NOT-FOUND))
      (existing-record (map-get? listen-records {user: tx-sender, track-id: track-id}))
      (reward (calculate-listen-reward listen-type stream-value))
      (current-count (match existing-record
        record (get listen-count record)
        u0))
      (current-value (match existing-record
        record (get total-value record)
        u0))
    )
    (asserts! (get is-minted track) ERR-INVALID-STATE)
    (asserts! (> stream-value u0) ERR-INVALID-AMOUNT)
    
    ;; Update listen record
    (map-set listen-records {user: tx-sender, track-id: track-id}
      {
        listen-count: (+ current-count u1),
        total-value: (+ current-value stream-value),
        last-listen: block-height,
        verification-score: reward
      })
    
    ;; Reward listener
    (map-set token-balances tx-sender 
      (+ (default-to u0 (map-get? token-balances tx-sender)) reward))
    (var-set total-supply (+ (var-get total-supply) reward))
    
    ;; Update council status
    (update-council-status tx-sender stream-value)
    (ok reward)
  )
)

;; Label sponsorship functionality
(define-public (sponsor-track 
  (label-name (string-ascii 64))
  (track-id uint)
  (sponsor-amount uint))
  (let
    (
      (track (unwrap! (map-get? tracks track-id) ERR-TRACK-NOT-FOUND))
      (existing-partner (map-get? label-partners tx-sender))
      (current-sponsored (match existing-partner
        partner (get total-sponsored partner)
        u0))
      (sponsor-balance (default-to u0 (map-get? token-balances tx-sender)))
    )
    (asserts! (not (get is-minted track)) ERR-TRACK-ALREADY-MINTED)
    (asserts! (> sponsor-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= sponsor-balance sponsor-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update partnership record
    (map-set label-partners tx-sender
      {
        name: label-name,
        total-sponsored: (+ current-sponsored sponsor-amount),
        active-deals: (match existing-partner
          partner (+ (get active-deals partner) u1)
          u1),
        reputation: u100
      })
    
    ;; Update track funding
    (map-set tracks track-id
      (merge track
        {
          current-funding: (+ (get current-funding track) sponsor-amount)
        }))
    
    ;; Transfer sponsorship tokens directly
    (map-set token-balances tx-sender (- sponsor-balance sponsor-amount))
    (map-set token-balances (as-contract tx-sender) 
      (+ (default-to u0 (map-get? token-balances (as-contract tx-sender))) sponsor-amount))
    (ok true)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-token-balance (user principal))
  (default-to u0 (map-get? token-balances user))
)

(define-read-only (get-track-info (track-id uint))
  (map-get? tracks track-id)
)

(define-read-only (get-rights-distribution (track-id uint))
  (map-get? rights-distribution track-id)
)

(define-read-only (get-vote-info (voter principal) (track-id uint))
  (map-get? votes {voter: voter, track-id: track-id})
)

(define-read-only (get-listen-stats (user principal) (track-id uint))
  (map-get? listen-records {user: user, track-id: track-id})
)

(define-read-only (get-council-status (user principal))
  (map-get? council-members user)
)

(define-read-only (get-label-info (label principal))
  (map-get? label-partners label)
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-global-stats)
  {
    total-supply: (var-get total-supply),
    next-track-id: (var-get next-track-id),
    yield-multiplier: (var-get yield-multiplier),
    streaming-volume: (var-get global-streaming-volume)
  }
)