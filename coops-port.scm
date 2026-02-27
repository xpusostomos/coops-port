(module coops-port (
<port> <input-port> <output-port> <binary-port>
     <binary-input-port> <binary-output-port>
     close closed? get-position set-position! 
     available flush! read! write! read-byte! write-byte!
     
     ;; Constants
     whence/beginning whence/current whence/end
     
     ;; Bytevector Implementation
     <bytevector-port> <bytevector-input-port> <bytevector-output-port>
     <bytevector-dynamic-output-port>
     make-bytevector-input-port
     make-bytevector-output-port
     make-bytevector-dynamic-output-port
     reset-input-data!
     truncate!
     get-output-u8vector
     get-data-length
     get-buffer-capacity
     set-buffer-capacity!
     
     ;; Buffered Decorators
     <buffered-port> <buffered-input-port> <buffered-output-port>
     make-buffered-input-port
     make-buffered-output-port
     fill!)				   

(import scheme coops srfi-4 (chicken base) (chicken port) (chicken memory))

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

(define-generic (flush! port))

(define-method (flush! (port <output-port>))
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
  (let* ((bv (make-u8vector 1 0)))
    (if (= (read! port bv 0 1) 1)
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

(define-generic (write-byte! port byte)
  @("writes the byte to the output stream"
	(@to "undefined")))

(define-method (write-byte! (port <binary-output-port>) byte)
  (let* ((bv (make-u8vector 1 0)))
    (u8vector-set! bv 0 byte)
    (write! port bv 0 1)
    (void)))


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

(define-method (reset-input-data! (port <bytevector-input-port>) data #!optional (start 0) (count #f))
 (let* ((data-len (u8vector-length data))
         (actual-count (or count (- data-len start)))
         (end (+ start actual-count)))
   (set! (slot-value port 'data) data)
   (set! (slot-value port 'start) start)
   (set! (slot-value port 'offset) start)
   (set! (slot-value port 'size) end)
   (set! (slot-value port 'limit) end)))
 
(define-method (read! (port <bytevector-input-port>) bytevector #!optional (start 0) (count #f))
  (let* ((data (slot-value port 'data))
         (offset (slot-value port 'offset))
         ;; The hard wall of the sandbox
         (limit (slot-value port 'limit)) 
         (requested (or count (- (u8vector-length bytevector) start)))
         (to-copy (min requested (max 0 (available port)))))
    (move-memory! data bytevector to-copy offset start)
    (set! (slot-value port 'offset) (+ offset to-copy))
    to-copy))

(define-method (read-byte! (port <bytevector-input-port>))
  (let ((offset (slot-value port 'offset)))
	(if (< offset (slot-value port 'size))
		(begin
		  (set! (slot-value port 'offset) (+ offset 1))
		  (u8vector-ref (slot-value port 'data) offset))
        #!eof)))

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
    
    (let* ((limit (slot-value port 'limit))
           (to-write (min requested (max 0 (- limit p)))))
      
      (move-memory! bytevector (slot-value port 'data) to-write start p)
      
      ;; 2. Update cursor
      (set! (slot-value port 'offset) (+ p to-write))
      
      ;; 3. Update High-Water Mark (size) only if we extended the content
      (when (> (slot-value port 'offset) (slot-value port 'size))
        (set! (slot-value port 'size) (slot-value port 'offset)))
        
      to-write)))

(define-method (write-byte! (port <bytevector-output-port>) byte)
  (let ((offset (slot-value port 'offset)))
	(ensure-capacity! port (+ offset 1))
	(if (< offset (slot-value port 'limit))
		(begin
		  (u8vector-set! (slot-value port 'data) offset byte)
		  (set! (slot-value port 'offset) (+ offset 1))
		  (when (> (slot-value port 'offset) (slot-value port 'size))
			(set! (slot-value port 'size) (slot-value port 'offset)))))))


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

(define initial-buffer-capacity 32)

(define (make-bytevector-dynamic-output-port #!optional (initial-cap initial-buffer-capacity))
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
    (set! (slot-value port 'data) (make-u8vector initial-buffer-capacity 0))))

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

(define-generic (set-buffer-capacity! port))


(define-method (set-buffer-capacity! (port <bytevector-dynamic-output-port>) #!optional (size initial-buffer-capacity))
  (let* ((hwm (slot-value port 'size))
         (current-limit (slot-value port 'limit))
         ;; The floor is the high-water mark; we cannot shrink past our data.
         (target-cap (max hwm size)))
    
    (unless (= target-cap current-limit)
      (let ((new-data (make-u8vector target-cap 0))
            (old-data (slot-value port 'data)))
        
        ;; Move all valid data (size) into the new allocation
        (move-memory! old-data new-data hwm 0 0)
        
        (set! (slot-value port 'data) new-data)
        (set! (slot-value port 'limit) target-cap)))
    
    ;; Return the relative limit
    (slot-value port 'limit)))

(define-generic (get-buffer-capacity port))

(define-method (get-buffer-capacity (port <bytevector-port>))
  (- (slot-value port 'limit) (slot-value port 'start)))

(define-generic (get-output-u8vector port))

(define-method (get-output-u8vector (port <bytevector-port>))
  (let* ((data (slot-value port 'data))
         (s (slot-value port 'start))
         (h (slot-value port 'size)))
    ;; Return a copy of the segment from start to the high-water mark
    (subu8vector data s h)))

(define-method (get-data-length (port <bytevector-port>))
  (- (slot-value port 'size) (slot-value port 'start)))

(define-class <buffered-port> (<output-port>)
  ((source/sink)          ;; Any port-like object that implements write! and flush!
   (buffer)        ;; The <bytevector-fixed-output-port> logic manager
   (raw-buffer)))  ;; The physical u8vector backing the buffer


(define-class <buffered-output-port> (<buffered-port> <binary-output-port> )
  )

(define (make-buffered-output-port source/sink #!optional (size 4096))
  (let ((raw (make-u8vector size 0)))
    (make <buffered-output-port>
          'source/sink source/sink
          'buffer (make-bytevector-output-port raw)
          'raw-buffer raw)))

(define-method (write! (port <buffered-output-port>) bytevector #!optional (start 0) (count #f))
  (let* ((requested  (or count (- (u8vector-length bytevector) start)))
         (buf        (slot-value port 'buffer))
         (source/sink       (slot-value port 'source/sink))
         ;; Use the relative capacity helper we defined earlier
         (free-space (- (get-buffer-capacity buf) (get-position buf))))
    
    (if (> requested free-space)
        ;; --- FAST PATH (Bypass) ---
        ;; If it won't fit, flush what we have and blast the rest to the source/sink.
        (begin
          (flush! port)
          (write! source/sink bytevector start requested))
        
        ;; --- SLOW PATH (Buffered) ---
        ;; If it fits, keep it in memory to aggregate small writes.
        (write! buf bytevector start requested))))


(define-method (flush! (port <buffered-output-port>))
  (let ((buf  (slot-value port 'buffer))
        (source/sink (slot-value port 'source/sink))
        (raw  (slot-value port 'raw-buffer)))
    (write! source/sink raw 0 (get-data-length buf))
    (truncate! buf 0)
    (flush! source/sink)))

(define-method (write-byte! (port <buffered-output-port>) byte)
  (let ((buf (slot-value port 'buffer)))
    ;; If the internal buffer is full, dump it to the sink
    (when (zero? (- (get-buffer-capacity buf) (get-position buf)))
      (flush! port))
    ;; Use the fast bytevector-output-port write-byte
    (write-byte! buf byte)))

(define-class <buffered-input-port> (<buffered-port> <binary-input-port> )
  )


(define (make-buffered-input-port source/sink #!optional (size 4096))
  (let ((raw (make-u8vector size 0)))
    (make <buffered-input-port>
          'source/sink source/sink
          'buffer (make-bytevector-input-port raw 0 0) ;; big buffer, but nothing there yet
          'raw-buffer raw)))


(define-method (read! (port <buffered-input-port>) bytevector #!optional (start 0) (count #f))
  (let* ((requested (or count (- (u8vector-length bytevector) start)))
         (buf       (slot-value port 'buffer))
         (cap       (u8vector-length (slot-value port 'raw-buffer)))
		 (drained (read! buf bytevector start requested))
		 (remaining (- requested drained)))
     (if (zero? remaining)
    	 drained
	     (+ drained
     	   (if (> remaining cap)
		        (read! (slot-value port 'source/sink) bytevector (+ start drained) remaining)
                (begin
				  (fill! port)
     	          (read! buf bytevector (+ start drained) remaining)))))))

(define-method (fill! (port <buffered-input-port>))
   (let ((raw-buf (slot-value port 'raw-buffer)))
     (reset-input-data!
	  (slot-value port 'buffer)
	  raw-buf
	  0
	    (read! (slot-value port 'source/sink) raw-buf 0 (u8vector-length raw-buf)))))

(define-method (read-byte! (port <buffered-input-port>))
  (let* ((buffer (slot-value port 'buffer))
		(c (read-byte! buffer)))
	(if (eof-object? c)
		(begin
		  (fill! port)
		  (read-byte! buffer))
		c)))


)


