(import test 
        coops 
        srfi-4
		srfi-1
		;; utf8
        (chicken base)
		(chicken file posix)
		(chicken locative)
		(chicken foreign)
        (chicken port)
		(chicken blob)
		(chicken process)
        (chicken memory)
		(chicken file)
        (chicken condition)
		(chicken random)
		(chicken io)
		(chicken string)
		(chicken bitwise)
        (only (chicken file) delete-file)
		coops-port)

(define random pseudo-random-integer)

(define (string->utf8 str)
  (blob->u8vector (string->blob str)))

(define (u8vector->string bv)
  (blob->string (u8vector->blob bv)))

(define large-data (make-u8vector 5000))

; Fill with a predictable pattern so we can detect mismatches
(do ((i 0 (+ i 1)))
    ((= i 5000))
  (u8vector-set! large-data i (modulo i 256)))

(define (run-input-stress-test port-gen source-data ops-count)
  (let* ((data-len (u8vector-length source-data))
         (port     (port-gen source-data))
         ;; Use your correct COOPS class check
         (buf-cap  (if (subclass? (class-of port) <buffered-input-port>)
                       (get-buffer-capacity (slot-value port 'buffer))
                       1024))
         (ref-pos  0))
    (do ((i 0 (+ i 1)))
        ((= i ops-count))
      (let ((op (pseudo-random-integer 4)))
        (cond 
         ;; OP 0: Read Byte
         ((= op 0)
          (let ((b1 (read-byte! port))
                (b2 (if (< ref-pos data-len) (u8vector-ref source-data ref-pos) #!eof)))
            (unless (eqv? b1 b2) (error "Stress: read-byte mismatch" b1 b2))
            (unless (eof-object? b1) (set! ref-pos (+ ref-pos 1)))))

         ;; OP 1: Peek Data
         ((= op 1)
          (let* ((max-p (min buf-cap (- data-len ref-pos)))
                 (count (if (<= max-p 0) 0 (+ 1 (pseudo-random-integer max-p)))))
            (when (> count 0)
              (let ((buf (make-u8vector count)))
                (peek! port buf 0 count)
                (do ((j 0 (+ j 1)))
                    ((= j count))
                  (let ((b1 (u8vector-ref buf j))
                        (b2 (u8vector-ref source-data (+ ref-pos j))))
                    (unless (eqv? b1 b2) (error "Stress: peek! data mismatch" b1 b2))))))))

         ;; OP 2: Bulk Read
         ((= op 2)
          (let* ((max-r (min 64 (- data-len ref-pos)))
                 (count (if (<= max-r 0) 0 (+ 1 (pseudo-random-integer max-r)))))
            (when (> count 0)
              (let* ((buf (make-u8vector count))
                     (n   (read! port buf 0 count)))
                (do ((j 0 (+ j 1)))
                    ((= j n))
                  (let ((b1 (u8vector-ref buf j))
                        (b2 (u8vector-ref source-data (+ ref-pos j))))
                    (unless (eqv? b1 b2) (error "Stress: read! data mismatch" b1 b2))))
                (set! ref-pos (+ ref-pos n))))))

         ;; OP 3: Random Seek
         ((= op 3)
          (let ((new-pos (pseudo-random-integer data-len)))
            (set-position! port new-pos)
            (set! ref-pos new-pos))))))
    #t))

(test-begin "Coop-Ports")

;;--- 1. Bytevector Input Ports ---
(test-group "1. Bytevector Input Ports"
  (let* ((source (u8vector 10 20 30 40 50 60))
         (in-port (make-bytevector-input-port source 2 3))) ;; Segment: [30, 40, 50]
    
    (test "Initial position is 0 (relative)" 0 (get-position in-port))
    (test "First byte read is correct (30)" 30 (read-byte! in-port))
    
    (test-group "1.1 Positioning & Seek"
      (set-position! in-port 0 whence/end)
      (test "Seek to end returns relative size (3)" 3 (get-position in-port))
      
      (set-position! in-port -1 whence/end)
      (test "Relative seek -1 from end is correct (50)" 50 (read-byte! in-port))
      
      (set-position! in-port 0 whence/beginning)
      (test "Seek back to beginning" 0 (get-position in-port)))
    
    (test "Available reporting" 3 (available in-port))))

;; --- 2. Output Ports (Fixed & Dynamic) ---
(test-group "2. Output Ports"
  
  (test-group "2.1 Fixed Sandbox"
    (let* ((dest (make-u8vector 10 0))
           (out (make-bytevector-output-port dest 5 3))) ;; Index 5,6,7
      (test "Write within capacity" 2 (write! out (u8vector 1 2)))
      (test "Short write on overflow" 1 (write! out (u8vector 3 4 5)))
      (test "Data correctly placed in raw vector" (u8vector 1 2 3) (subu8vector dest 5 8))))

  (test-group "2.2 Dynamic Expansion"
    (let ((dyn (make-bytevector-dynamic-output-port 4)))
      (write! dyn (u8vector 101 102 103 104 105))
      (test "Capacity doubled" #t (>= (get-buffer-capacity dyn) 8))
      (test "Position correct" 5 (get-position dyn))
      (truncate! dyn 2)
      (test "Truncate affects size/available" 2 (get-data-length dyn)))))

;; --- 3. Buffered Infrastructure (The "Heavy Lifters") ---
(test-group "3. Buffered Output Path"
  (let* ((sink (make-bytevector-dynamic-output-port))
         (b-out (make-buffered-output-port sink 10))) ;; Tiny 10-byte buffer
    
    (test "Small write stays in buffer (Sink empty)" 0 (get-data-length sink))
    (write! b-out (u8vector 1 2 3))

	(test-group "3.1 Bypass Optimization"
	  (write! b-out (make-u8vector 15 99))
	  ;; Use your API: get-output-u8vector returns the whole logical segment
	  (let ((result (get-output-u8vector sink)))
		(test "Bypass wrote large chunk directly to sink" 18 (u8vector-length result))
		(test "Check specific byte in bypassed data" 99 (u8vector-ref result 5))))))
	
(test-group "4. Buffered Input Path (Auto-Refill & Bypass)"
  (let* ((raw-data (u8vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (source (make-bytevector-input-port raw-data))
         (b-in (make-buffered-input-port source 5)) ;; 5-byte buffer
         (target (make-u8vector 20 0)))
    
    (test-group "4.1 Auto-Fill Logic"
      (test "Read-byte! triggers initial fill" 1 (read-byte! b-in))
      (test "Buffer now has remaining 4 bytes" 4 (available (slot-value b-in 'buffer))))

    (test-group "4.2 Tiered Bypass Logic"
      ;; We have 4 bytes in buffer. We ask for 10.
      ;; Logic: Drain 4 from buffer, Bypass read 6 from source.
      (let ((read-count (read! b-in target 0 10)))
        (test "Total bytes read matches request" 10 read-count)
        (test "Correct data sequence after bypass" 2 (u8vector-ref target 0))
        (test "Source cursor advanced correctly" 11 (get-position source))))
    
    (test-group "4.3 EOF Handling"
      (let ((final-read (read! b-in target 0 100))) ;; Read way past end
        (test "Handles EOF gracefully" 4 final-read)))))


(test-group "5. Single-Byte I/O (Specialized Methods)"
  
  (test-group "5.1 Bytevector Single-Byte"
    (let* ((data (u8vector 10 20 30))
           (in (make-bytevector-input-port data))
           (out (make-bytevector-output-port (make-u8vector 3 0))))
      (test "read-byte! matches sequence" 10 (read-byte! in))
      (test "read-byte! increments offset" 1 (get-position in))
      (write-byte! out 99)
      ;; Note: get-output-u8vector returns a copy of the written segment
      (test "write-byte! updates data" (u8vector 99) (get-output-u8vector out))
      (test "write-byte! updates size" 1 (get-data-length out))))

  (test-group "5.2 Buffered Auto-Refill (read-byte!)"
    (let* ((source-data (u8vector 1 2 3 4 5 6))
           (source (make-bytevector-input-port source-data))
           (b-in (make-buffered-input-port source 2))) ;; Tiny 2-byte buffer
      (test "First read-byte! triggers fill" 1 (read-byte! b-in))
      (test "Second read-byte! from existing buffer" 2 (read-byte! b-in))
      (test "Third read-byte! triggers second fill" 3 (read-byte! b-in))
      (test "Available reflects refill state" 1 (available (slot-value b-in 'buffer)))))

  (test-group "5.3 Buffered Auto-Flush (write-byte!)"
    (let* ((sink (make-bytevector-dynamic-output-port))
           (b-out (make-buffered-output-port sink 2))) ;; Tiny 2-byte buffer
      (write-byte! b-out 10)
      (write-byte! b-out 20)
      (test "Buffer full but not yet flushed" 0 (get-data-length sink))
      (write-byte! b-out 30) ;; Should trigger flush of [10, 20]
      (test "Sink received flushed data" 2 (get-data-length sink))
      (test "Flushed data is correct" (u8vector 10 20) (get-output-u8vector sink))
      (flush! b-out)
      (test "Final flush captures last byte" 3 (get-data-length sink))
      (test "Complete data correct" (u8vector 10 20 30) (get-output-u8vector sink))))

  (test-group "5.4 Interleaved read! and read-byte!"
    (let* ((source-data (u8vector 1 2 3 4 5 6 7 8 9 10))
           (source (make-bytevector-input-port source-data))
           (b-in (make-buffered-input-port source 4))
           (target (make-u8vector 2 0)))
      ;; Sequence of actions
      (read-byte! b-in)       ;; Reads 1, manager offset 1
      (read! b-in target 0 2) ;; Reads 2, 3, manager offset 3
      (test "Data from interleaved read! is correct" 
      (u8vector->list (u8vector 2 3)) 
      (u8vector->list target))
      (test "Next read-byte! is correct" 4 (read-byte! b-in))
      (test "Buffer manager now empty" 0 (available (slot-value b-in 'buffer)))))
  )

(test-group "6. Raw File I/O"
  (let* ((filename "test_io.bin")
         (test-data (u8vector 65 66 67 68 69)) ;; "ABCDE"
         (out-raw (make-file-output-port filename (bitwise-ior open/write open/create open/truncate)))
         (out-buf (make-buffered-output-port out-raw 2))) ;; Tiny buffer to force flushes
    
    (write! out-buf test-data)
    (close out-buf) ;; Should flush and close FD
    
    (let* ((in-raw (make-file-input-port filename open/read))
           (in-buf (make-buffered-input-port in-raw 2))
           (result (make-u8vector 5 0)))
      (read! in-buf result)
      (test "File round-trip matches" (u8vector->list test-data) (u8vector->list result))
      (close in-buf))))

(test-group "7. FFI Locative & Offset Validation"
  (let* ((filename "locative_test.bin")
         ;; Create a file containing [10, 20, 30]
         (out (make-file-output-port filename (bitwise-ior open/write open/create open/truncate)))
         (dummy-data (u8vector 10 20 30)))
    
    (write! out dummy-data)
    (close out)

    (let* ((in (make-file-input-port filename open/read))
           ;; Target vector initialized to zeros
           (target (make-u8vector 5 0)))
      
      ;; READ TEST: Read 3 bytes from file into 'target' starting at index 2
      ;; Target should become: #u8(0 0 10 20 30)
      (read! in target 2 3)
      
      (test "Data lands at correct offset via locative" 
            '(0 0 10 20 30) 
            (u8vector->list target))
      (close in))))


(test-group "8. Base <file-port> Finalizer"
  (let ((port-ptr #f))
    (let ((p (make-file-input-port "locative_test.bin" open/read)))
      (set! port-ptr p)
      (test "Port starts open" #f (closed? p)))
    
    (set! port-ptr #f)
    ;; Trigger a full GC to reap the orphaned port object
    (import (only (chicken gc) gc))
    (gc #t) 
    
    (let ((p2 (make-file-input-port "locative_test.bin" open/read)))
      (close p2)
      (test "Manual close neutralizes finalizer safely" #t (closed? p2)))))

(test-group "9. Buffered Input Peek & Shuffle"
  (let* ((data (u8vector 1 2 3 4 5 6 7 8 9 10))
         ;; Create a tiny 6-byte buffer to force refills/shuffles quickly
         (src  (make-bytevector-input-port data))
         (bport (make-buffered-input-port src 6))
         (target (make-u8vector 4 0)))

    ;; 1. Initial read to move the offset
    ;; Buffer: [1, 2, 3, 4, 5, 6], offset: 0 -> 3
    (test "Initial read of 3 bytes" 3 (read! bport (make-u8vector 3)))
    (test "Current offset is 3" 3 (get-position (slot-value bport 'buffer)))

    ;; 2. Peek for 4 bytes. 
    ;; Only 3 bytes (4, 5, 6) are available in the 6-byte buffer.
    ;; This MUST trigger a slide: [4, 5, 6] moves to [0, 1, 2]
    ;; Then a fill! appends [7, 8, 9] to [3, 4, 5]
    (test "Peek 4 bytes across shuffle boundary" 4 (peek! bport target 0 4))
    (test "Peeked data is correct" '(4 5 6 7) (u8vector->list target))

    ;; 3. Verify Idempotency: The offset should still be at the '4'
    (test "Offset did not move after peek" 4 (read-byte! bport))
    
    ;; 4. Verify the rest of the stream is intact
    (let ((remainder (make-u8vector 5 0)))
      (read! bport remainder)
      (test "Stream remains contiguous after shuffle" 
            '(5 6 7 8 9) 
            (u8vector->list remainder)))))

(test-group "10 Peek-Byte Validation"
  (let* ((data (u8vector 100 101 102))
         (src  (make-bytevector-input-port data))
         (bport (make-buffered-input-port src 2)))
    (test "Peek-byte returns 100" 100 (peek-byte! bport))
    (test "Peek-byte is idempotent" 100 (peek-byte! bport))
    (read-byte! bport)
    (test "After read, peek-byte returns 101" 101 (peek-byte! bport))))

(test-group "11. Cross-Implementation Stress Tests"
  (let ((large-data (list->u8vector (map (lambda (x) (random 256)) (make-list 1000)))))

    (test "Raw Bytevector Input Stress" #t
          (run-input-stress-test 
           (lambda (d) (make-bytevector-input-port d)) 
           large-data 5000))

    (test "Buffered Input (Small Buffer) Stress" #t
          (run-input-stress-test 
           (lambda (d) (make-buffered-input-port (make-bytevector-input-port d) 16)) 
           large-data 5000))

    (test "Buffered Input (Standard Buffer) Stress" #t
          (run-input-stress-test 
           (lambda (d) (make-buffered-input-port (make-bytevector-input-port d) 1024)) 
           large-data 5000))))

(test-group "12. File-Input Stress Tests"
  (let* ((size 5000)
         (filename "stress-test.bin")
         (large-data (list->u8vector (map (lambda (x) (pseudo-random-integer 256)) (make-list size)))))
    
    ;; 1. Setup file
    (let ((out (make-file-output-port filename)))
      (write! out large-data)
      (close out))

    ;; 2. Test with fresh ports per run
    (test "Buffered File Input (Tiny 13b Buffer)" #t
          (run-input-stress-test 
           (lambda (d) (make-buffered-input-port (make-file-input-port filename) 13)) 
           large-data 2000))

    (test "Buffered File Input (Page-size 4k Buffer)" #t
          (run-input-stress-test 
           (lambda (d) (make-buffered-input-port (make-file-input-port filename) 4096)) 
           large-data 2000))

    (when (file-exists? filename) (delete-file filename))))

(test-group "Minimal Peek-Shuffle Failure Case"
  (let* ((data (u8vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (src  (make-bytevector-input-port data))
         ;; 1. Create a port with a 10-byte buffer
         (bport (make-buffered-input-port src 10))
         (peek-buf (make-u8vector 5 0)))

    ;; 2. Move cursor to index 7. 
    ;; Buffer manager now has: pos=7, len=10, available=3.
    ;; The internal data is [7, 8, 9].
    (read! bport (make-u8vector 7))
    
    ;; 3. Request a Peek of 5 bytes. 
    ;; Since available (3) < requested (5), the port MUST:
    ;;   a. Slide [7, 8, 9] from index 7 to index 0.
    ;;   b. Update manager to pos=0, len=3.
    ;;   c. Call fill! to get more data (fetching 10, 11, 12, etc.)
    (peek! bport peek-buf 0 5)

    (test "The peeked data should be (7 8 9 10 11)" 
          '(7 8 9 10 11) 
          (u8vector->list peek-buf))

    (test "The next read-byte should still be 7 (Idempotency)" 
          7 
          (read-byte! bport))))
(test-group "The Bypass-Peek Synchronization Bug"
  (let* ((data (u8vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
         (src  (make-bytevector-input-port data))
         (bport (make-buffered-input-port src 4)) ;; Small buffer to force bypass
         (target (make-u8vector 10 0)))
    (read-byte! bport) ;; Buffer fills [1 2 3 4], pos=1, avail=3
    ;; Request 10 bytes: 3 from buffer, 7 via Bypass from source.
    ;; If the Bypass doesn't reset the manager, the manager still thinks it has data!
    (read! bport target 0 10) 
    (test "The next byte must be 12" 12 (peek-byte! bport))))

(test-group "The Stale Window Seek Bug"
  (let* ((data (u8vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
         (src  (make-bytevector-input-port data))
         (bport (make-buffered-input-port src 5)) ;; 5-byte buffer
         (target (make-u8vector 10 0)))
    (read-byte! bport) ;; Fills [1 2 3 4 5], pos=1, avail=4. Source is at 5.
    (read! bport target 0 10) ;; Drains 4, Bypasses 6. Source is now at 11.
    ;; The buffer is logically exhausted, but NOT reset. 
    ;; Window-start = 11 - 5 = 6. Window-end = 11.
    (set-position! bport 7) ;; Seek to absolute 7. 
    ;; This is inside the [6, 11] window! The port will treat this as a Cache Hit.
    (test "Byte at pos 7 should be 8" 8 (read-byte! bport))))


(test-group "The Refill-Shift Logic Failure"
  (let* ((data (u8vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
         ;; 10 byte buffer
         (bport (make-buffered-input-port (make-bytevector-input-port data) 10))
         (trash (make-u8vector 5 0)))

    ;; 1. Fill buffer: [0-9], Source at 10.
    (fill! bport) 

    ;; 2. Drain 7 bytes: Buffer has left. Source still at 10.
    (read! bport trash 0 7)

    ;; 3. Read 5 more bytes. 
    ;; Drained=3. Remaining=2.
    ;; 2 < 10, so NO BYPASS. 
    ;; fill! is called. Buffer was empty? No, 3 were left.
    ;; If fill! appends, we are fine. 
    ;; IF fill! resets because we were "at the wall", the window shifts.
    (read! bport trash 0 5)

    ;; 4. Now Seek to 8.
    ;; Current Source Pos is 12 (10 from fill + 2 read).
    ;; Buffer has 8 bytes left (starts at file pos 12).
    ;; Old Math: 12 - (available 8) = 4.
    ;; Math says: "8 is between 4 and 12. CACHE HIT!"
    ;; Reality: The buffer contains bytes [12, 13, 14...]. 
    ;; Position 8 is GONE from the buffer.
    (set-position! bport 8)
    
    (test "Should be 8" 8 (read-byte! bport))))


(test-group "The Stale Window Stress Trigger"
  (let* ((data (u8vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
         ;; Small 8-byte buffer to force frequent refills
         (bport (make-buffered-input-port (make-bytevector-input-port data) 8))
         (target (make-u8vector 10 0)))

    ;; 1. Prime the buffer (Reads 0-7, Source moves to 8)
    (read-byte! bport) 

    ;; 2. Trigger a Bypass Read (Size 10 > Capacity 8)
    ;; This reads bytes 1-10 directly from source.
    ;; Source moves to 11. Buffer manager is reset.
    (read! bport target 0 10)

    ;; 3. Now the "Ghost" setup:
    ;; If we call available or peek now, fill! might trigger.
    ;; Source is at 11. Buffer fills with.
    (peek-byte! bport)

    ;; 4. The Killing Seek:
    ;; target-pos 7. 
    ;; Old Math: source-pos(19) - dl(8) = 11. Is 7 in? No.
    ;; BUT, if the math sees dl as 16 or source as 11... 
    ;; We want to verify that seeking to a previously bypassed byte 
    ;; doesn't accidentally pull from the NEW buffer content.
    (set-position! bport 12)
    
    (let ((result (read-byte! bport)))
      (test "Byte at pos 12 should be 12" 12 result))))


(test-group "COOPS-to-CHICKEN Bridge"
  
  ;; 1. TEST: Raw File Port -> CHICKEN io
  (test-group "Raw File Port Bridge"
    (let* ((path "bridge_test.tmp")
           (_ (with-output-to-file path (lambda () (display "Line 1\nLine 2\nLine 3"))))
           (coops-file (make-file-input-port path))
           (c-handle (coops-port->chicken-port coops-file)))
      
      (test "Identity check: COOPS -> CHICKEN -> COOPS"
            coops-file
              (chicken-port->coops-port c-handle))
	  
      (test "CHICKEN read-line (Line 1)" "Line 1" (read-line c-handle))
      (test "CHICKEN read-line (Line 2)" "Line 2" (read-line c-handle))
      
      (close-input-port c-handle)
      (test "COOPS side reflects close" #t (closed? coops-file))))
  
  ;; 2. TEST: Buffered Input -> High-level Peek/Read
  (test-group "Buffered Decorator Bridge"
    (let* ((data (string->utf8 "0123456789ABCDEF"))
           (source (make-bytevector-input-port data))
           (b-port (make-buffered-input-port source 8)) ;; 8 byte buffer
           (c-handle (coops-port->chicken-port b-port)))
	  
      (test "Standard peek-char (binary)" #\0 (peek-char c-handle))
      (test "Standard read-char" #\0 (read-char c-handle))
      
      ;; Move the logical cursor
      (read-char c-handle) ;; #\1
      (read-char c-handle) ;; #\2
      
      (test "CHICKEN read-string (5 bytes)" "34567" (read-string 5 c-handle))
      (test "COOPS logical position sync" 8 (get-position b-port))))

  ;; 3. TEST: Dynamic Output -> Standard Display/Write
  (test-group "Dynamic Output Bridge"
    (let* ((dyn-out (make-bytevector-dynamic-output-port 4))
           (c-handle (coops-port->chicken-port dyn-out)))
      
      (display "Hello " c-handle)
      (write-char #\W c-handle)
      (display "orld" c-handle)
      
      (test "Content verify" "Hello World" 
            (u8vector->string (get-output-u8vector dyn-out)))))
  
  ;; 4. TEST: Complex Interop (Chicken's copy-port)
  (test-group "The copy-port Interop"
    (let* ((src-data (string->utf8 "The quick brown fox"))
           (in-coops (make-bytevector-input-port src-data))
           (out-coops (make-bytevector-dynamic-output-port))
           (in-handle (coops-port->chicken-port in-coops))
           (out-handle (coops-port->chicken-port out-coops)))
      
      ;; Use CHICKEN's native copy-port utility
      (copy-port in-handle out-handle)
      
      (test "Full copy success" "The quick brown fox" 
            (u8vector->string (get-output-u8vector out-coops))))))


(test-group "The Zero-Copy Magic Test"
  (let* ((source-data (u8vector 65 66 67 68 69)) 
         (port (make-bytevector-input-port source-data))
         (target-str (make-string 5 #\.)))
    
    (read! port target-str 0 5)
    
    (test "String is now ABCDE" "ABCDE" target-str)
    
    ;; Create alias NOW. If this works, it proves we didn't need 
    ;; to manually update the string; the bytes were just there.
    (let ((alias (blob->u8vector (string->blob target-str))))
      (test "Alias reflects new bytes" 
            '(65 66 67 68 69) 
            (u8vector->list alias)))))

(test-group "The Zero-Copy Blob Test"
  (let* ((source-data (u8vector 72 105 33)) ;; "Hi!"
         (port (make-bytevector-input-port source-data))
         (target-blob (make-blob 3)))

    (read! port target-blob 0 3)
    
    (test "Alias reflected blob update" 
          '(72 105 33) 
          (u8vector->list (blob->u8vector target-blob)))
	))

(define (get-coops-port-content obj)
  ;; (if (subclass? (class-of obj) <buffered-port>)
  ;; 	  (flush! obj))
  (cond
   ;; 1. If it's a decorator, recurse into the underlying port
    ((subclass? (class-of obj) <buffered-port>) 
     (get-coops-port-content (slot-value obj 'source/sink)))
    
    ;; 2. If it's a memory-backed port, extract the active slice
    ((subclass? (class-of obj) <bytevector-port>) 
     (let ((data (slot-value obj 'data))
           (start (slot-value obj 'start))
           (offset (slot-value obj 'offset)))
       (subu8vector data start (+ start offset))))
    
    ;; 3. If it's a file, read the current content back from disk

	((subclass? (class-of obj) <file-port>)
	 (sync! obj)
	 (let* ((fd (slot-value obj 'fd))
			(saved-pos (get-position obj))
			;; Ground truth: use the current cursor as the size
			(actual-size saved-pos) 
			;; Create a BLOB (not a u8vector) to satisfy file-read
			(target-blob (make-blob actual-size)))
	   (set-file-position! fd 0 whence/beginning)
	   ;; file-read returns (list buffer bytes-read)
	   (file-read fd actual-size target-blob)
	   (set-file-position! fd saved-pos whence/beginning) 
	   ;; Convert the mutated blob back to a u8vector for your test
	   (blob->u8vector target-blob)))
	 
    (else #f)))

(define (test-input-bridge-polymorphic port-gen source-data iterations)
  (let* ((coops-obj (port-gen source-data))
         (c-port    (coops-port->chicken-port coops-obj))
         (data-len  (u8vector-length source-data))
         (ref-pos   0))
    (do ((i 0 (+ i 1)))
        ((= i iterations))
      (let ((op (pseudo-random-integer 3)))
        (cond
         ((= op 0) (let ((c (read-char c-port))) 
                     (unless (eof-object? c) (set! ref-pos (+ ref-pos 1)))))
         ((= op 1) (let* ((n (min 32 (- data-len ref-pos)))
                          (buf (make-string n))
                          (read (read-string! n buf c-port)))
                     (set! ref-pos (+ ref-pos read))))
         ((= op 2) (peek-char c-port)))))
    (let ((final-pos (get-position coops-obj)))
      (close-input-port c-port)
      (= final-pos ref-pos))))

(define (test-output-bridge-polymorphic port-gen size)
  (let* ((coops-obj (port-gen))
         (c-port    (coops-port->chicken-port coops-obj)))
    (do ((i 0 (+ i 1)))
        ((= i size))
      (write-char #\A c-port))
    (flush-output c-port)
    (let ((content (get-coops-port-content coops-obj)))
      (close-output-port c-port)
      (and (u8vector? content)
           (= (u8vector-length content) size)
           (every (lambda (b) (= b 65)) (u8vector->list content))))))


(test-group "Bridge: copy-port Interop Stress"
  (let* ((size (* 1024 50)) ;; 50KB
         (raw-in (list->u8vector (map (lambda (x) (pseudo-random-integer 256)) (make-list size))))
         (src-coops (make-bytevector-input-port raw-in))
         (dst-coops (make-bytevector-dynamic-output-port))
         (src-chicken (coops-port->chicken-port src-coops))
         (dst-chicken (coops-port->chicken-port dst-coops)))
    
    ;; This triggers Hook 7 on src and Hook 3 on dst repeatedly
    (copy-port src-chicken dst-chicken)
    
    (let ((result (get-output-u8vector dst-coops)))
      (test "Copy-port total length" size (u8vector-length result))
      (test "Copy-port integrity" (u8vector->list raw-in) (u8vector->list result)))
    
    (close-input-port src-chicken)
    (close-output-port dst-chicken)))

  (test-group "Bridge: Boundary Conditions"
  
  (test-group "Exact Buffer Match"
    (let* ((data (u8vector 49 50 51 52))
           (in (coops-port->chicken-port (make-bytevector-input-port data)))
           (buf (make-string 4)))
      ;; Should fill exactly 4 bytes
      (test "Read exact size" 4 (read-string! 4 buf in))
	  (test "Content match" '(49 50 51 52) (map char->integer (string->list buf)))))
  
  (test-group "Over-read Handling"
    (let* ((data (u8vector 65 66)) ;; "AB"
           (in (coops-port->chicken-port (make-bytevector-input-port data)))
           (buf (make-string 10 #\.)))
      ;; User asks for 10, but only 2 exist.
      (test "Short read-string!" 2 (read-string! 10 buf in))
      (test "Buffer partially mutated" "AB........" buf)))

  (test-group "Zero-length Ops"
    (let* ((in (coops-port->chicken-port (make-bytevector-input-port (u8vector 1 2 3)))))
      (test "Zero read-string!" 0 (read-string! 0 (make-string 5) in))
      (test "Position unchanged after zero-read" 0 (get-position (chicken-port->coops-port in))))))

(test-group "Bridge: UTF-8 Binary Integrity"
  (let* ((utf8-str "λ is a Greek letter")
         (raw-bytes (string->blob utf8-str))
         (in-coops (make-bytevector-input-port (blob->u8vector raw-bytes)))
         (in-chicken (coops-port->chicken-port in-coops)))
    
    ;; read-line in CHICKEN is locale-aware but handles standard binary strings
    (test "UTF-8 Roundtrip via Bridge" 
          utf8-str 
          (read-line in-chicken))
    (close-input-port in-chicken)))

(test-group "Final Bridge Torture Test (Polymorphic Composition)"
  
  (let ((data (make-u8vector 2000))
        (fn "bridge_stress.bin"))
    ;; Fill with a predictable pattern
    (do ((i 0 (+ i 1))) ((= i 2000)) (u8vector-set! data i (modulo i 256)))

    (test-group "Input Bridge Targets"
      ;; 1. Raw Bytevector
      (test "Raw Bytevector" #t 
            (test-input-bridge-polymorphic make-bytevector-input-port data 3000))
      
      ;; 2. Buffered Bytevector
      (test "Buffered Bytevector" #t 
            (test-input-bridge-polymorphic 
              (lambda (d) (make-buffered-input-port (make-bytevector-input-port d) 64)) 
              data 3000))
      
      ;; 3. Raw File Port
      (test "Raw File Port" #t 
            (begin
              (with-output-to-file fn (lambda () (display (u8vector->string data))))
              (test-input-bridge-polymorphic (lambda (_) (make-file-input-port fn)) data 3000)))

      ;; 4. Buffered File Port (Full Composition)
      (test "Buffered File Port" #t 
            (let ((res (test-input-bridge-polymorphic 
                         (lambda (_) (make-buffered-input-port (make-file-input-port fn) 128)) 
                         data 3000)))
              (when (file-exists? fn) (delete-file fn))
              res)))

    (test-group "Output Bridge Targets"
      ;; 1. Raw Dynamic
      (test "Dynamic Output" #t 
            (test-output-bridge-polymorphic make-bytevector-dynamic-output-port 2000))
      
      ;; 2. Buffered Dynamic
      (test "Buffered Dynamic" #t 
            (test-output-bridge-polymorphic 
              (lambda () (make-buffered-output-port (make-bytevector-dynamic-output-port) 64)) 
              2000))
      
      ;; 3. Buffered File Output
      (test "Buffered File Output" #t 
            (begin
              (let ((res (test-output-bridge-polymorphic 
                           (lambda () (make-buffered-output-port (make-file-output-port fn (bitwise-ior open/read-write open/create open/truncate)) 256)) 
                           2000)))
                (when (file-exists? fn) (delete-file fn))
                res))))))

(test-group "13. Non-Seekable POSIX Ports (Pipes)"
  (receive (in-fd out-fd) (create-pipe)
    (let ((p-in  (make-posix-input-port in-fd))
          (p-out (make-posix-output-port out-fd))
          (test-msg (string->utf8 "Pipe data"))
          (target (make-u8vector 20 0)))

      (test-group "13.1 Basic Pipe I/O"
        (test "Write to posix-output-port" 9 (write! p-out test-msg))
        ;; We don't need to flush pipes, but let's ensure it doesn't crash
        (test "Flush is no-op" (void) (flush! p-out))
        
        (test "Read from posix-input-port" 9 (read! p-in target 0 9))
        (test "Data integrity" "Pipe data" (u8vector->string (subu8vector target 0 9))))

      (test-group "13.3 Cleanup"
        (close p-in)
        (close p-out)
        (test "Input port closed" #t (closed? p-in))
        (test "Output port closed" #t (closed? p-out))))))

(define (run-io-stress-test port-gen ops-count)
  (let* ((port (port-gen))
         (ref-buffer (make-u8vector 10000 0)) ;; Our "ground truth"
         (ref-size 0)
         (ref-pos 0))
    (do ((i 0 (+ i 1)))
        ((= i ops-count))
      (let ((op (pseudo-random-integer 3)))
        (cond
         ;; OP 0: Write Data
         ((= op 0)
          (let* ((count (+ 1 (pseudo-random-integer 64)))
                 (data (make-u8vector count)))
            ;; Fill with pattern
            (do ((j 0 (+ j 1))) ((= j count)) 
              (u8vector-set! data j (pseudo-random-integer 256)))
            (write! port data 0 count)
            ;; Update reference
            (move-memory! data ref-buffer count 0 ref-pos)
            (set! ref-pos (+ ref-pos count))
            (set! ref-size (max ref-size ref-pos))))

         ;; OP 1: Seek to random existing point
         ((= op 1)
          (let ((target (pseudo-random-integer (max 1 ref-size))))
            (set-position! port target)
            (set! ref-pos target)))

         ;; OP 2: Read and Compare
         ((= op 2)
          (let* ((avail (- ref-size ref-pos))
                 (count (if (<= avail 0) 0 (+ 1 (pseudo-random-integer (min 64 avail))))))
            (when (> count 0)
              (let ((buf (make-u8vector count 0)))
                (read! port buf 0 count)
                (do ((j 0 (+ j 1))) ((= j count))
                  (unless (= (u8vector-ref buf j) (u8vector-ref ref-buffer (+ ref-pos j)))
                    (error "IO-Stress: Read mismatch at pos" (+ ref-pos j))))
                (set! ref-pos (+ ref-pos count)))))))))
    (close port)
    #t))

(test-group "14. Input-Output (Duplex) Ports"

  (test-group "14.1 Bytevector Duplex"
    (let* ((data (make-u8vector 100 0))
           (port (make-bytevector-input-output-port data)))
      (write! port (string->utf8 "Hello"))
      (set-position! port 0)
      (let ((buf (make-u8vector 5)))
        (read! port buf 0 5)
        (test "Read back from BV-IO" "Hello" (u8vector->string buf)))))

  (test-group "14.2 File Duplex (R/W mode)"
    (let* ((fn "duplex_test.bin")
           (port (make-file-input-output-port fn (bitwise-ior open/read-write open/create open/truncate))))
      (write! port (u8vector 10 20 30 40))
      (set-position! port 1)
      (test "File IO Read at offset" 20 (read-byte! port))
      (write-byte! port 99) ;; Overwrite position 2 (where 30 was)
      (set-position! port 2)
      (test "File IO Verification of overwrite" 99 (read-byte! port))
      (close port)
      (when (file-exists? fn) (delete-file fn))))

  (test-group "14.3 Polymorphic IO Stress"
    (test "Bytevector IO Stress" #t
          (run-io-stress-test 
            (lambda () (make-bytevector-input-output-port (make-u8vector 20000 0))) 
            1000))
    
    (test "File IO Stress" #t
          (let ((fn "io_stress.bin"))
            (let ((res (run-io-stress-test 
                        (lambda () (make-file-input-output-port fn (bitwise-ior open/read-write open/create open/truncate))) 
                        500)))
              (when (file-exists? fn) (delete-file fn))
              res)))))



(test-end "Coop-Ports")
