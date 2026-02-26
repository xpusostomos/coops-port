(import coops srfi-4 (chicken base) (chicken port) (chicken memory))

(define-class <port> ()
  ((chicken-port)
   (buffer initform: (make-u8vector 1))))

(define-generic (close port)
  @("Close port"
	(port "port")
	(@to "undefined")))

(define-method (close (port <port>))
  (void))

(define-generic (closed? port)
  @("Is the port closed?"
	(port "port")
	(@to "boolean")))

(define-method (closed? (port <port>))
  #f)

(define-generic (get-position port)
  @("Returns an object representing the location in the stream.
If it's a number, it means bytes from the beginning. If it's some other object
it is something opaque you can pass to set-position! If the port doesn't support
this feature, it returns #f"
	(port "port")
	(@to "number or object or #f")))

(define-method (get-position (port <port>))
  #f)

(define whence/beginning 'beginning)
(define whence/current 'current)
(define whence/end 'end)

(define-generic (set-position! port position)
  @("Reset the position in the stream"
	(port "port")
	(position "object returned from get-position")
	(whence "If position is an integer, seek relative to that location.
If position is not an integer, whence is ignored and the stream is restored state represented by the opaque object.")
	(@to "#f if unsupported by this stream")))

(define-method (set-position! (port <port>) position #!optional (whence whence/beginning))
  #f)

(define-class <input-port> (<port>))

(define-generic (available port)
  @("Is data available?"
	(port "port")
	(@to "exact number")))

(define-method (available (port <input-port>))
  0)

(define-class <output-port> (<port>))

(define-generic (flush port))

(define-method (flush (port <output-port>))
  (void))

(define-class <textual-port> (<port>))

(define-class <binary-port> (<port>))

(define-class <binary-input-port> (<binary-port> <input-port>))

(define-generic (read! port bytevector))

(define-generic (read-byte! port)
  @("Read one octet from the port"
	(port "port")
	(@to "octet or #eof")))


(define-method (read-byte! (port <binary-input-port>))
  (let ((bv (slot-value port 'buffer)))
    (if (= (read! port bv 0 1) 1)
;    (if (= (read! port bv) 1)
        (u8vector-ref bv 0)
        #!eof)))

(define-class <binary-output-port> (<binary-port> <output-port>))

(define-generic (write! port bytevector)
  ;#!optional (start 0) (count #f))
  @("It is an error if the following conditions on the arguments are not met: start and count are non-negative exact integers, and bytevector is a bytevector whose length is at least start + count.

The write! procedure writes up to count bytes from bytevector starting at index start to the byte sink. The write! procedure returns the number of bytes that it wrote, as an exact integer."
	(port "port")
	(bytevector "location to store")
	(start "offset into bytevector")
	(count "max number of bytes to read")
	(@to "exact integer"))
  )

(define-generic (write-byte port byte)
  @("writes the byte to the output stream"
	(@to "undefined")))

(define-method (write-byte (port <binary-output-port>) byte)
  (let ((bv (slot-value port 'buffer)))
    (u8vector-set! bv 0 byte)
    (write! port bv 0 1)
    (void)))

(define-class <buffered-port> (<port>))

(define-class <buffered-input-port> (<input-port> <buffered-port>))

(define-class <buffered-output-port> (<output-port> <buffered-port>))

(define-class <bytevector-port> (<binary-input-port>)
  ((start initform: 0)
   (offset initform: 0)
   (size initform: 0)
   (limit initform: 0)
   (data)))

(define-class <bytevector-input-port> (<binary-input-port> <bytevector-port>))

(define (make-bytevector-input-port data #!optional (start 0) (count #f))
  (let* ((data-len (u8vector-length data))
         (actual-count (or count (- data-len start)))
         (end (+ start actual-count)))
    (make <bytevector-input-port> 
          'data  data
          'start start
          'offset start
          'size  end
          'limit end)))

(define-method (read! (port <bytevector-input-port>) bytevector #!optional (start 0) (count #f))
  (let* ((data (slot-value port 'data))
         (p (slot-value port 'offset))
         ;; The hard wall of the sandbox
         (boundary (slot-value port 'limit)) 
         
         (requested (or count (- (u8vector-length bytevector) start)))
         
         ;; We can read all the way to the 'limit', 
         ;; regardless of the 'size' high-water mark.
         (available (- boundary p))
         (to-copy (min requested (max 0 available))))
    
    (move-memory! data bytevector to-copy p start)
    (set! (slot-value port 'offset) (+ p to-copy))
    
    to-copy))

(define-method (available (port <bytevector-port>))
  (max 0 (- (slot-value port 'size) (slot-value port 'offset))))

(define-method (get-position (port <bytevector-port>))
  (- (slot-value port 'offset) (slot-value port 'start)))


(define-method (set-position! (port <bytevector-port>) position #!optional (whence whence/beginning))
  (let* ((s (slot-value port 'start))
         (l (slot-value port 'limit))
         (h (slot-value port 'size))
         ;; Calculate the new ABSOLUTE position
         (target-pos (cond
                       ((eq? whence whence/beginning) (+ s position))
                       ((eq? whence whence/current)   (+ (slot-value port 'offset) position))
                       ((eq? whence whence/end)       (+ h position))
                       (else (slot-value port 'offset)))))
    ;; Clamp absolute pos within [start, limit]
    (set! (slot-value port 'offset) (max s (min l target-pos)))
    ;; Return the relative position to the user
    (get-position port)))


(define-class <bytevector-output-port> (<binary-output-port> <bytevector-port>)
  )

(define (make-bytevector-output-port data #!optional (start 0) (count #f))
  (let* ((data-len (u8vector-length data))
         (actual-count (or count (- data-len start)))
         (end (+ start actual-count)))
    (make <bytevector-output-port>
          'data   data
          'start  start
          'offset start
          'size   start  ;; Initially 0 bytes written
          'limit  end)))

(define-method (write! (port <bytevector-output-port>) bytevector #!optional (start 0) (count #f))
  (let* ((requested (or count (- (u8vector-length bytevector) start)))
         (p (slot-value port 'offset)))
    
    ;; 1. Prepare the sandbox (Dynamic: grows 'limit'; Fixed: does nothing)
    (ensure-capacity! port (+ p requested))
    
    (let* ((boundary (slot-value port 'limit))
           (to-write (min requested (max 0 (- boundary p)))))
      
      (move-memory! bytevector (slot-value port 'data) to-write start p)
      
      ;; 2. Update cursor
      (set! (slot-value port 'offset) (+ p to-write))
      
      ;; 3. Update High-Water Mark (size) only if we extended the content
      (when (> (slot-value port 'offset) (slot-value port 'size))
        (set! (slot-value port 'size) (slot-value port 'offset)))
        
      to-write)))

(define-generic (ensure-capacity! port req))

;; Fixed ports do nothing—they just let write! clamp to existing size
(define-method (ensure-capacity! (port <bytevector-output-port>) req)
  (void))

(define-generic (truncate! (port <bytevector-output-port>)))

(define-method (truncate! (port <bytevector-output-port>) length)
  (let* ((s (slot-value port 'start))
         (l (slot-value port 'limit))
         (new-hwm (+ s length))
		 (new-size (max s (min l new-hwm))))
    ;; Clamp high-water mark to [start, limit]
    (set! (slot-value port 'size) new-size)
    ;; Pull cursor back if it's now out of bounds
    (when (> (slot-value port 'offset) new-size)
      (set! (slot-value port 'offset) new-size))
    ;; Return relative size
    (- new-size s)))


(define-class <bytevector-dynamic-output-port> (<bytevector-output-port>)
  )

(define (make-bytevector-dynamic-output-port #!optional (initial-cap 32))
  (let ((data (make-u8vector initial-cap 0)))
    (make <bytevector-dynamic-output-port>
          'data   data
          'start  0
          'offset 0
          'size   0
          'limit  initial-cap)))

(define-method (initialize-instance (port <bytevector-dynamic-output-port>))
  (call-next-method)
  ;; Ensure we have at least a tiny buffer to start with for the doubling math
  (unless (slot-value port 'data)
    (set! (slot-value port 'data) (make-u8vector 32 0))))

(define-generic (calculate-growth port requested-size))

(define-method (calculate-growth (port <bytevector-dynamic-output-port>) req)
  ;; Default strategy: double the current capacity
  (max req (* (u8vector-length (slot-value port 'data)) 2)))

(define-method (ensure-capacity! (port <bytevector-dynamic-output-port>) req)
  (let* ((data (slot-value port 'data))
         (current-cap (u8vector-length data)))
    
    (when (> req current-cap)
      (let* ((new-cap (calculate-growth port req))
             (new-data (make-u8vector new-cap 0)))
        ;; Copy all existing data up to the high-water mark
        (move-memory! data new-data (slot-value port 'size) 0 0)
        (set! (slot-value port 'data) new-data)))
    
    ;; Push the "Hard Wall" (limit) to match the request or new capacity
    (when (> req (slot-value port 'limit))
      (set! (slot-value port 'limit) (u8vector-length (slot-value port 'data))))))

(define-method (truncate! (port <bytevector-dynamic-output-port>) new-size)
  ;; 1. Use the factored-out logic to ensure physical memory exists
  (ensure-capacity! port new-size)
  
  ;; 2. Capacity is already handled by ensure-capacity! 
  ;; but we call next-method to update the 'size' and 'offset' slots
  (call-next-method))








;; TEST CASES:

(print "--- Testing Input Segment ---")
(let* ((source (u8vector 10 20 30 40 50 60)) ;; The "Big" vector
       ;; Create a port looking only at 30, 40, 50 (indices 2, 3, 4)
       (in-port (make-bytevector-input-port source 2 3)))
  
  (print "Initial Pos (Relative): " (get-position in-port)) ;; Should be 0
  (print "Byte 1: " (read-byte! in-port))                  ;; Should be 30
  
  (set-position! in-port 0 whence/end) 
  (print "Pos at End: " (get-position in-port))            ;; Should be 3
  
  (set-position! in-port -1 whence/end)
  (print "Last Byte: " (read-byte! in-port))               ;; Should be 50
  
  (print "Available: " (available in-port)))               ;; Should be 0



(print "\n--- Testing Fixed Output Sandbox ---")
(let* ((dest (make-u8vector 10 0))
       ;; Sandbox indices 5 to 7 (3 bytes capacity)
       (out-port (make-bytevector-output-port dest 5 3)))
  
  (print "Initial Size: " (available out-port))            ;; Should be 0
  (write! out-port (u8vector 1 2))                         ;; Write 2 bytes
  (print "Pos after 2: " (get-position out-port))          ;; Should be 2
  
  ;; Attempt to write 5 bytes into a 3-byte sandbox
  (let ((written (write! out-port (u8vector 3 4 5 6 7))))
    (print "Short Write Count: " written))                 ;; Should be 1
    
  (print "Final Dest: " dest))                             ;; Should have 1, 2, 3 at indices 5, 6, 7

(print "\n--- Testing Dynamic Expansion ---")
(let ((dyn-port (make-bytevector-dynamic-output-port 4)))   ;; Start tiny (4 bytes)
  
  (write! dyn-port (u8vector 101 102 103 104 105))         ;; Write 5 bytes (triggers growth)
  (print "New Capacity (Limit): " (slot-value dyn-port 'limit)) 
  (print "Relative Pos: " (get-position dyn-port))         ;; Should be 5
  
  (set-position! dyn-port 0 whence/beginning)
  (truncate! dyn-port 2)                                   ;; Shrink logical size to 2
  (print "Available to Read: " (available dyn-port)))      ;; Should be 2


(print "\n--- Testing Truncate Up ---")
(let ((sparse-port (make-bytevector-dynamic-output-port 10)))
  (write! sparse-port (u8vector 255))                      ;; Write 1 byte
  (truncate! sparse-port 100)                              ;; Jump to size 100
  (print "High-Water Mark: " (slot-value sparse-port 'size)) ;; Should be 100
  (set-position! sparse-port 0 whence/end)
  (print "Pos at new End: " (get-position sparse-port)))   ;; Should be 100

;; END test

