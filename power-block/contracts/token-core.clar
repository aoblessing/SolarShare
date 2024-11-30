;; title: token-core
;; version: 1.2
;; description: Enhanced Core NFT contract for SolarShare power tokens with improved error handling

;; Constants for contract control
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-POWER-CAPACITY u100) 
(define-constant MAX-POWER-CAPACITY u10000)
(define-constant MAX-LOCK-PERIOD u52560)
(define-constant MIN-LOCATION-LENGTH u5)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-PARAMETERS (err u103))
(define-constant ERR-INSTALLATION-NOT-VERIFIED (err u104))
(define-constant ERR-INSUFFICIENT-CAPACITY (err u105))
(define-constant ERR-INSTALLATION-INACTIVE (err u106))
(define-constant ERR-POWER-CAPACITY-BOUNDS (err u107))
(define-constant ERR-TOKEN-LOCKED (err u108))
(define-constant ERR-INVALID-ADDRESS (err u109))
(define-constant ERR-INVALID-LOCATION (err u110))
(define-constant ERR-SELF-TRANSFER (err u111))
(define-constant ERR-LOCK-PERIOD-EXCEEDED (err u112))
(define-constant ERR-ZERO-CAPACITY (err u113))

;; Data Variables
(define-data-var token-id-nonce uint u0)
(define-data-var total-installations uint u0)
(define-data-var contract-paused bool false)

;; NFT Definition
(define-non-fungible-token power-share uint)

;; Data Maps
(define-map token-metadata
    uint
    {
        installation-id: uint,
        power-capacity: uint,
        minted-at: uint,
        last-transfer: uint,
        locked-until: uint,
        power-used: uint,
        token-uri: (optional (string-utf8 256))
    }
)

(define-map installation-details
    uint
    {
        location: (string-ascii 50),
        total-capacity: uint,
        active-tokens: uint,
        created-at: uint,
        verified: bool,
        active: bool,
        maintainer: principal,
        last-maintenance: uint,
        installation-uri: (optional (string-utf8 256))
    }
)

(define-map banned-addresses 
    principal 
    bool
)

(define-map power-usage
    { token-id: uint, cycle: uint }
    {
        used: uint,
        remaining: uint,
        last-update: uint
    }
)

;; Private Functions
(define-private (validate-location (location (string-ascii 50)))
    (if (>= (len location) MIN-LOCATION-LENGTH)
        (ok true)
        ERR-INVALID-LOCATION))

(define-private (validate-power-capacity (power-capacity uint))
    (if (is-eq power-capacity u0)
        ERR-ZERO-CAPACITY
        (if (and 
                (>= power-capacity MIN-POWER-CAPACITY)
                (<= power-capacity MAX-POWER-CAPACITY))
            (ok true)
            ERR-POWER-CAPACITY-BOUNDS)))

(define-private (validate-lock-period (period uint))
    (if (<= period MAX-LOCK-PERIOD)
        (ok true)
        ERR-LOCK-PERIOD-EXCEEDED))

(define-private (validate-transfer-recipient (sender principal) (recipient principal))
    (if (is-eq sender recipient)
        ERR-SELF-TRANSFER
        (ok true)))

(define-private (assert-is-owner)
    (if (is-eq tx-sender CONTRACT-OWNER)
        (ok true)
        ERR-NOT-AUTHORIZED))

(define-private (assert-not-banned (address principal))
    (if (is-banned address)
        ERR-NOT-AUTHORIZED
        (ok true)))

(define-private (assert-contract-not-paused)
    (if (var-get contract-paused)
        ERR-NOT-AUTHORIZED
        (ok true)))

(define-private (assert-installation-active (installation-id uint))
    (match (map-get? installation-details installation-id)
        installation (if (get active installation)
            (ok true)
            ERR-INSTALLATION-INACTIVE)
        ERR-NOT-FOUND))

;; Read-Only Functions
(define-read-only (get-token-info (token-id uint))
    (ok (map-get? token-metadata token-id)))

(define-read-only (get-installation-info (installation-id uint))
    (ok (map-get? installation-details installation-id)))

(define-read-only (get-token-owner (token-id uint))
    (ok (nft-get-owner? power-share token-id)))

(define-read-only (is-banned (address principal))
    (default-to false (map-get? banned-addresses address)))

(define-read-only (get-contract-status)
    (ok (var-get contract-paused)))

(define-read-only (get-power-usage (token-id uint) (cycle uint))
    (ok (map-get? power-usage { token-id: token-id, cycle: cycle })))

;; Public Functions
(define-public (set-contract-pause (pause bool))
    (begin
        (try! (assert-is-owner))
        (asserts! (not (is-eq pause (var-get contract-paused))) ERR-INVALID-PARAMETERS)
        (ok (var-set contract-paused pause))))

(define-public (set-address-ban (address principal) (banned bool))
    (begin
        (try! (assert-is-owner))
        (asserts! (not (is-eq address CONTRACT-OWNER)) ERR-INVALID-PARAMETERS)
        (asserts! (not (is-eq banned (is-banned address))) ERR-INVALID-PARAMETERS)
        (ok (map-set banned-addresses address banned))))

(define-public (register-installation 
        (location (string-ascii 50))
        (total-capacity uint)
        (maintainer principal)
        (installation-uri (optional (string-utf8 256))))
    (let
        ((installation-id (+ (var-get total-installations) u1)))
        (try! (assert-contract-not-paused))
        (try! (assert-is-owner))
        (try! (assert-not-banned maintainer))
        (try! (validate-location location))
        (try! (validate-power-capacity total-capacity))
        
        (asserts! (not (is-eq maintainer CONTRACT-OWNER)) ERR-INVALID-PARAMETERS)
        
        (map-set installation-details installation-id
            {
                location: location,
                total-capacity: total-capacity,
                active-tokens: u0,
                created-at: block-height,
                verified: false,
                active: true,
                maintainer: maintainer,
                last-maintenance: block-height,
                installation-uri: installation-uri
            })
        (var-set total-installations installation-id)
        (ok installation-id)))

(define-public (mint-power-share 
        (recipient principal)
        (installation-id uint)
        (power-capacity uint)
        (token-uri (optional (string-utf8 256))))
    (let
        ((token-id (+ (var-get token-id-nonce) u1))
         (installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        
        (try! (assert-contract-not-paused))
        (try! (assert-not-banned recipient))
        (try! (validate-power-capacity power-capacity))
        (try! (assert-installation-active installation-id))
        
        (asserts! (not (is-eq recipient tx-sender)) ERR-SELF-TRANSFER)
        (asserts! (get verified installation) ERR-INSTALLATION-NOT-VERIFIED)
        
        (let ((new-total-capacity (+ power-capacity 
                                    (* (get active-tokens installation) power-capacity))))
            (asserts! (<= new-total-capacity (get total-capacity installation))
                ERR-INSUFFICIENT-CAPACITY))
        
        (try! (nft-mint? power-share token-id recipient))
        
        (map-set token-metadata token-id
            {
                installation-id: installation-id,
                power-capacity: power-capacity,
                minted-at: block-height,
                last-transfer: block-height,
                locked-until: u0,
                power-used: u0,
                token-uri: token-uri
            })
        
        (map-set installation-details installation-id
            (merge installation 
                {active-tokens: (+ (get active-tokens installation) u1)}))
        
        (map-set power-usage { token-id: token-id, cycle: block-height }
            {
                used: u0,
                remaining: power-capacity,
                last-update: block-height
            })
        
        (var-set token-id-nonce token-id)
        (ok token-id)))

(define-public (transfer-token (token-id uint) (recipient principal))
    (let 
        ((owner (unwrap! (nft-get-owner? power-share token-id) ERR-NOT-FOUND))
         (token-data (unwrap! (map-get? token-metadata token-id) ERR-NOT-FOUND)))
        
        (try! (assert-contract-not-paused))
        (try! (assert-not-banned recipient))
        (try! (validate-transfer-recipient tx-sender recipient))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (>= block-height (get locked-until token-data)) ERR-TOKEN-LOCKED)
        
        (try! (nft-transfer? power-share token-id tx-sender recipient))
        
        (map-set token-metadata token-id
            (merge token-data {last-transfer: block-height}))
        
        (ok true)))

(define-public (lock-token (token-id uint) (lock-period uint))
    (let ((owner (unwrap! (nft-get-owner? power-share token-id) ERR-NOT-FOUND))
          (token-data (unwrap! (map-get? token-metadata token-id) ERR-NOT-FOUND)))
        (try! (validate-lock-period lock-period))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set token-metadata token-id
            (merge token-data 
                {locked-until: (+ block-height lock-period)}))
        (ok true)))

(define-public (update-power-usage (token-id uint) (power-used uint))
    (let ((owner (unwrap! (nft-get-owner? power-share token-id) ERR-NOT-FOUND))
          (usage-data (unwrap! (map-get? power-usage { token-id: token-id, cycle: block-height }) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (<= (+ power-used (get used usage-data)) (get remaining usage-data)) ERR-INSUFFICIENT-CAPACITY)
        
        (map-set power-usage { token-id: token-id, cycle: block-height }
            (merge usage-data {
                used: (+ power-used (get used usage-data)),
                last-update: block-height
            }))
        (ok true)))

(define-public (update-maintenance (installation-id uint))
    (let ((installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        (asserts! (or 
            (is-eq tx-sender (get maintainer installation))
            (is-eq tx-sender CONTRACT-OWNER)) 
            ERR-NOT-AUTHORIZED)
        
        (map-set installation-details installation-id
            (merge installation 
                {last-maintenance: block-height}))
        (ok true)))

(define-public (set-installation-active (installation-id uint) (active bool))
    (let ((installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        (try! (assert-is-owner))
        (map-set installation-details installation-id
            (merge installation {active: active}))
        (ok true)))

;; Initialize contract
(begin
    (var-set token-id-nonce u0)
    (var-set total-installations u0)
    (var-set contract-paused false))
