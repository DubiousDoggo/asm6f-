;; iNES header
.inesprg 1  ; one bank of program code
.ineschr 1  ; one bank of picture data
.inesmap 0  ; use mapper 0
.inesmir 1  ; nametable mirroring: 0 horizontal, 1 vertical

;; CPU ADDRESSES

PPU_CTRL       = $2000 
PPU_MASK       = $2001
PPU_STATUS     = $2002
PPU_OAM_ADDR   = $2003
PPU_OAM_DATA   = $2004
PPU_SCROLL     = $2005
PPU_ADDRESS    = $2006
PPU_DATA       = $2007

SND_SQUARE1    = $4000
SND_SQUARE2    = $4004
SND_TRIANGLE   = $4008
SND_NOISE      = $400C
SND_DMC        = $4010

OAM_DMA        = $4014

APU_STATUS     = $4015

JOYPAD_PORT1   = $4016
JOYPAD_PORT2   = $4017

;; PPU ADDRESSES

PPU_NAMETABLE1 = $2000
PPU_ATTRIBUTE  = $23C0
PPU_NAMETABLE2 = $2400
PPU_ATTRIBUTE2 = $27C0
PPU_PALETTE    = $3F00

;; RAM ADDRESSES

BUFFER_OAM = $0200
BUFFER_PPU = $0300 

;; ZERO PAGE

.enum $0
  VAR_A .dsb 1
  VAR_B .dsb 1
  VAR_C .dsb 1
  VAR_D .dsb 1

  PTR_A .dsw 1
  PTR_B .dsw 1
  PTR_C .dsw 1

  NMI_STATUS       .dsb 1
  BUFFER_PPU_CTRL  .dsb 1
  BUFFER_PPU_MASK  .dsb 1
  SCROLL_X         .dsb 1
  SCREEN_X         .dsb 1
  SCROLL_Y         .dsb 1
  SCROLL_DIRECTION .dsb 1
  GLOBAL_TIMER     .dsb 1
  JOYPAD1_STATE    .dsb 1
  JOYPAD1_TIMER_A  .dsb 1
  JOYPAD1_TIMER_B  .dsb 1

  OBJECT_ALLOCATION .dsb 1
  OBJECT_DATA       .dsb 32

.ende

.ignorenl
;; BUTTON MASKS

BUTTON_A        = %10000000
BUTTON_B        = %01000000
BUTTON_SELECT   = %00100000
BUTTON_START    = %00010000
BUTTON_UP       = %00001000
BUTTON_DOWN     = %00000100
BUTTON_LEFT     = %00000010
BUTTON_RIGHT    = %00000001

;; SPRITE ATTRIBUTE MASKS

SPRITE_VFLIP    = %10000000
SPRITE_HFLIP    = %01000000
SPRITE_PRIORITY = %00100000
SPRITE_PALETTE  = %00000011
  ;;               ||||||||
  ;;               ||||||++- Color Palette of sprite. Choose which set of 4 colors to use (4-7)
  ;;               |||+++--- Unimplemented
  ;;               ||+------ Priority (0: in front of background; 1: behind background)
  ;;               |+------- Flip sprite horizontally
  ;;               +-------- Flip sprite vertically

;; NMI_STATUS MASKS


NMI_SPINNING   = %00000001
NMI_OAM_DMA    = %00000010
NMI_PPU_BUFFER = %00000100
  ;;              ||||||||
  ;;              |||||||+- Is the main code waiting for NMI?
  ;;              ||||||+-- Should OAM DMA happen next NMI?
  ;;              |||||+--- Should the PPU buffer be transfered next NMI?
  ;;              +++++---- Unused  
.endinl

.org $C000 ; with mapper 0, ROM bank 0 starts at $C000
RESET: ; reset vector
  SEI  ; disable IRQs
  CLD  ; clear decimal mode

  ;; Clear RAM
  LDA #$00
  LDX #$FF
  - INX 
    STA #$00, x
    CPX #$FF
    BNE -

  ;; Clear OAM buffer
  LDA #$FE
  LDX #$FF
  - INX
    STA #BUFFER_OAM, x
    CPX #$FF
    BNE -
  
  ;; Initialize variables
  LDA #$00
  STA SCROLL_X
  STA SCREEN_X
  STA SCROLL_Y

  ;; Initialize PPU registers
  LDA #%10010000
  ;;    ||||||||
  ;;    ||||||++- Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
  ;;    |||||+--- VRAM address increment per CPU read/write of PPUDATA (0: add 1, going across; 1: add 32, going down)
  ;;    ||||+---- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000; ignored in 8x16 mode)
  ;;    |||+----- Background pattern table address (0: $0000; 1: $1000)
  ;;    ||+------ Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
  ;;    |+------- PPU master/slave select (0: read backdrop from EXT pins; 1: output color on EXT pins)
  ;;    +-------- Generate an NMI at the start of vblank (0: off; 1: on)
  STA BUFFER_PPU_CTRL
  STA PPU_CTRL
  
  LDA #%00011110
  ;;    ||||||||
  ;;    |||||||+- Grayscale (0: normal color; 1: AND all palette entries
  ;;    |||||||   with 0x30, effectively producing a monochrome display;
  ;;    |||||||   note that colour emphasis STILL works when this is on!)
  ;;    ||||||+-- Disable background clipping in leftmost 8 pixels of screen
  ;;    |||||+--- Disable sprite clipping in leftmost 8 pixels of screen
  ;;    ||||+---- Enable background rendering
  ;;    |||+----- Enable sprite rendering
  ;;    ||+------ Intensify reds (and darken other colors)
  ;;    |+------- Intensify greens (and darken other colors)
  ;;    +-------- Intensify blues (and darken other colors)  
  STA BUFFER_PPU_MASK
  STA PPU_MASK

  JSR LoadBackgroundData
  JSR LoadPaletteData

  LDA #<PlayerObject
  STA PTR_A
  LDA #>PlayerObject
  STA PTR_A + 1
  JSR LoadObject
  
  LDA #(NMI_SPINNING | NMI_OAM_DMA)
  STA NMI_STATUS
    
SPIN:
;; MAIN LOOP
  LDA NMI_STATUS
  AND #NMI_SPINNING
  BNE SPIN ; Wait for NMI
  
  JSR ReadInput
  
  ;; Tick objects
  LDA OBJECT_ALLOCATION
  LDY #$00
  - ROR A
    BCC +
      PHA    ; save register
      TYA    ; slot number
      PHA    ; save index
      ASL A  ; multiply by 8 for slot offset
      ASL A
      ASL A
      TAX    ; x = data slot offset
      LDA OBJECT_DATA, x ; tick pointer low byte
      STA PTR_C
      INX 
      LDA OBJECT_DATA, x ; tick pointer high byte
      STA PTR_C + 1
      DEX    ; x = data slot offset
      JMP ++ ; indirect subroutine jump
     -- JMP (PTR_C)
     ++ JSR -- 
      PLA   ; restore registers
      TAY
      PLA
  + INY
    CPY #$08
    BNE -


  ;; reset spinning
  LDA NMI_STATUS
  ORA #NMI_SPINNING
  STA NMI_STATUS
  JMP SPIN

;; NMI INTERRUPT
NMI:
  PHA ; back up A register
  LDA NMI_STATUS    
  AND #NMI_SPINNING ; if the main code was not spinning immediately return
  BNE +
    PLA ; restore A register
    RTI ; return from interrupt
+ TXA ; back up registers
  PHA
  TYA
  PHA

  LDA BUFFER_PPU_CTRL
  STA PPU_CTRL
  LDA BUFFER_PPU_MASK
  STA PPU_MASK

  ;; SPRITE DMA
  ;; The fastest and easiest way to transfer your sprites to memory is using DMA (Direct Memory Access). 
  ;; This just means a block of RAM is copied from CPU memory to the PPU sprite memory.
  ;; The on-board RAM space from $0200-02FF is usually used for this purpose. To start the transfer, 
  ;; two bytes need to be written to the PPU ports $2003 (PPU_OAM_ADDR) and $4014 (OAM_DMA)
  ;; Like all graphics updates, this needs to be done at the beginning of vblank.

  LDA NMI_STATUS
  AND #NMI_OAM_DMA ; check if OAM DMA should be done
  BEQ +
    LDA #<BUFFER_OAM ; set the low byte ($00) of the BUFFER_OAM address
    STA PPU_OAM_ADDR  
    LDA #>BUFFER_OAM ; set the high byte ($02) of the BUFFER_OAM address and start the transfer
    STA OAM_DMA       
+ 
  JSR LoadBackgroundColumn

  ;; PPU BUFFER
  ;; The PPU buffer is a list of packets to be transfered to the PPU.
  ;; Each packet has the following format:
  ;;
  ;;   1 bytes - length of data (0 = no more data)
  ;;   2 bytes - the target PPU address (HHLL)
  ;;   1 bytes - PPU_CTRL flags to set
  ;;   1 bytes - PPU_CTRL flags to clear (inverted)
  ;;           - the data to transfer (number of bytes determined by the length)

  ;; idfk whats going on with this but it dont work

;  LDA NMI_STATUS
;  AND #NMI_PPU_BUFFER   ; check if PPU buffer should be read
;  JMP + 
;  BEQ +
;  LDX #$00
;--
;  LDY BUFFER_PPU, x     ; start of a new packet
;  CPY #$00              ; if its length is 0 we're done
;  BEQ +                 
;  LDA PPU_STATUS        ; reset PPU latch
;  INX                   
;  LDA BUFFER_PPU, x     ; write high byte of PPU address
;  STA PPU_ADDRESS       
;  INX                   
;  LDA BUFFER_PPU, x     ; write low byte of PPU address
;  STA PPU_ADDRESS       
;  INX
;  LDA BUFFER_PPU_CTRL   ; write PPU control
;  ORA BUFFER_PPU, x
;  INX
;  AND BUFFER_PPU, x
;  STA PPU_CTRL
;- INX                   ; write Y bytes of data
;  LDA BUFFER_PPU, x
;  STA PPU_DATA
;  DEY
;  CPY #$00
;  BEQ --
;  JMP - 
;+ LDA BUFFER_PPU_CTRL   ; restore PPU control 
;  STA PPU_CTRL


  ;; BACKGROUND SCROLLING
  ;; PPU port $2005 (PPU_SCROLL) is used for background scrolling.
  ;; It is written to twice, once for x and y position.
  LDA PPU_STATUS
  LDA SCROLL_X 
  STA PPU_SCROLL
  LDA SCROLL_Y
  STA PPU_SCROLL

  INC GLOBAL_TIMER

  LDA NMI_STATUS    
  AND #~NMI_SPINNING ; clear NMI_SPINNING flag
  STA NMI_STATUS

  PLA ; restore registers
  TAY
  PLA
  TAX
  PLA
  RTI ; Return from interrupt

;; SUBROUTINES
  
;; PALETTES
  ;; Before putting any graphics on the screen, you first need to set the color palette.
  ;; There are two color palettes, each 16 bytes. One is used for the background and one is used for sprites.
  ;; The byte in the palette corresponds to one of the 64 base colors the NES can display.
  ;;
  ;; Palettes Start at PPU address $3F00 and $3F10. To set this address, PPU address port $2006 (PPU_ADDRESS) is used.
  ;; The port must be written twice, once for the high byte then for the low byte.
  ;;
  ;; This code tells the PPU to set its address to $3F00 (PPU_PALETTE).
  ;; Then the PPU data port at $2007 (PPU_DATA) is ready to accept data. 
  ;; The first write will go to the address set ($3F00),
  ;; then the PPU will automatically increment the address after each read or write.

LoadPaletteData:
    LDA PPU_STATUS    ; read PPU status to reset latch
    LDA #>PPU_PALETTE ; write high byte ($3F) to PPU 
    STA PPU_ADDRESS    
    LDA #<PPU_PALETTE ; write low byte ($00) to PPU 
    STA PPU_ADDRESS   

    ;; write 32 bytes of palette data
    LDX #$00
    - LDA PaletteData, x
      STA PPU_DATA       
      INX
      CPX #$20           
      BNE -
    
    RTS

LoadBackgroundData:
  ;; Background data is stored in the PPU nametables from address $2000 to $23BF and $2800 to $2FBF 
  ;; Each of the 960 bytes represents one 8x8 tile for a total of 32x30 tiles.
  ;; NTSC will crop the top and bottom 8 pixels, PAL displays the full resolution.
  ;; The data can be written in the same was as the palette data.   

  LDA #<BackgroundData
  STA PTR_B
  LDA #>BackgroundData
  STA PTR_B + 1

  LDA BUFFER_PPU_CTRL  ; set vertical PPU addressing and nametable offset
  ORA #%00000100       
  AND #%11111100
  STA PPU_CTRL

  LDA #$00
  STA VAR_A              ; column number
 -test LDA PPU_STATUS       ; read PPU status to reset latch
    LDA #>PPU_NAMETABLE1 ; write high byte to PPU
    STA PPU_ADDRESS  
    LDA VAR_A            ; write low byte to PPU (column number)
    STA PPU_ADDRESS
    
    LDY #$00             ; load column ($1E bytes) of tile data
    - LDA (PTR_B), y
      STA PPU_DATA
      INY
      CPY #$1E
      BNE -
    
    LDA (PTR_B), y 
    STA PTR_A      ; attribute data pointer low
    INY
    LDA (PTR_B), y 
    STA PTR_A + 1  ; attribute data pointer high

    LDA VAR_A ; divide by 4 to get attribute column
    LSR A     ; only load attribute data on multiples of 4
    BCS +     
    LSR A     
    BCS +     
      CLC
      ADC #<PPU_ATTRIBUTE
      STA VAR_B
      LDY #$00
      - LDA PPU_STATUS
        LDA #>PPU_ATTRIBUTE
        STA PPU_ADDRESS
        
        LDA VAR_B
        STA PPU_ADDRESS
        
        LDA (PTR_A), y
        STA PPU_DATA
        INY
        LDA (PTR_A), y
        STA PPU_DATA
        INY

        LDA VAR_B
        CLC
        ADC #$08
        STA VAR_B

        CPY #$08
        BNE -    

  + LDA PTR_B ; move PTR_B to next column (+$20)
    CLC
    ADC #$20
    STA PTR_B
    BCC +     ; handle carry
      INC PTR_B + 1
  + INC VAR_A ; increment column number
    LDA VAR_A
    CMP #$20
    BNE -test

  LDA BUFFER_PPU_CTRL  ; reset PPU addressing direction and offset
  STA PPU_CTRL

  RTS


LoadBackgroundColumn:
  ;; transfer background data to PPU 

    LDA BUFFER_PPU_CTRL
    ORA #%00000100      ; vertical VRAM direction
    STA PPU_CTRL

    LDA PPU_STATUS      ; reset latch

    LDA SCREEN_X        ; determine which nametable to write to
    AND #%00000001
    EOR SCROLL_DIRECTION
    BNE +
      LDA #>PPU_NAMETABLE1 
    JMP ++
  +   LDA #>PPU_NAMETABLE2
 ++ STA PPU_ADDRESS     ; PPU high byte
    STA VAR_A           ; save for attribute data calculation

    LDA SCROLL_X        ; divide scroll by 8 to get column
    LSR A
    LSR A
    LSR A       
    STA PPU_ADDRESS     ; PPU low byte
    STA VAR_B           ; save for attribute data calculation
    
    ;; Now find the column in memory to load, this should be the 16 bit value 
    ;; = ( (SCREEN_X + SCROLL_DIRECTION) * (32*32) ) + ((SCROLL_X / 8) * 32) + BackgroundData
    ;; = ( (SCREEN_X + SCROLL_DIRECTION)*256 + (SCROLL_X & %11111000) ) << 2 + BackgroundData
    ;; and needs to end up in PTR_B (low byte first)

    ; doing some of the logic here in the accumulator to save cycles
    ; accumulator shift is 2 cycles, zero page shift is 5

    LDA SCREEN_X 
    CLC
    ADC SCROLL_DIRECTION
    STA PTR_B + 1
    
    LDA SCROLL_X
    AND #%11111000
    ASL A
    ROL PTR_B + 1
    ASL A
    ROL PTR_B + 1
    CLC 
    ADC #<BackgroundData
    STA PTR_B
    LDA PTR_B + 1
    ADC #>BackgroundData
    STA PTR_B + 1

    LDY #$00 ; load 30 bytes of nametable data
    - LDA (PTR_B), y 
      STA PPU_DATA
      INY
      CPY #$1E     
      BNE -

    ;; attribute data
    LDA VAR_A
    CLC
    ADC #$03 ; attribute offset from nametable high byte
    STA VAR_A

    LDA VAR_B
    LSR A
    LSR A
    CLC
    ADC #$C0 ; attribute offset from nametable high byte
    STA VAR_B
    
    LDA (PTR_B), y ; attribute data low
    STA PTR_A
    INY
    LDA (PTR_B), y ; attribute data high
    STA PTR_A + 1
   
    LDY #$00
    - LDA PPU_STATUS
      LDA VAR_A
      STA PPU_ADDRESS
      LDA VAR_B
      STA PPU_ADDRESS
      
      LDA (PTR_A), y
      STA PPU_DATA
      INY
      LDA (PTR_A), y
      STA PPU_DATA
      INY

      LDA VAR_B
      CLC
      ADC #$08
      STA VAR_B

      CPY #$08
      BNE - 

    LDA BUFFER_PPU_CTRL ; restore VRAM direction
    STA PPU_CTRL

    RTS


;; ATTRIBUTE TABLES
  ;; The colors used by each background tile is determined by the attribute table.
  ;; The arribute table follows directly after the nametable for 64 bytes ($23C0).

;; SPRITES
  ;; Anything that moves separately from the background will be made of sprites. 
  ;; A sprite is an 8x8 pixel tile that the PPU renders anywhere on the screen.
  ;; Generally, objects are made from multiple sprites next to each other. The PPU has enough internal memory for 64 sprites. 
  ;; The memory is separate from all other video memory and cannot be expanded.
  ;; This data will be copied into the PPU sprite data via DMA each frame during NMI


;; OBJECTS
  ;; With this setup, 8 objects can be loaded and each object can have 8 up to sprites
  ;; Each object has a pointer to a tick routine and an offset in OAM
LoadObject:
  ;; PTR_A holds the object data address
  LDA OBJECT_ALLOCATION
  CMP #%11111111   ; if there are no slots available return
  BEQ +
    
    LDX #$FF         ; find the first available slot
    - INX
      ROR A
      BCS -   
    STX VAR_A        ; number of the first available slot
    SEC              ; set bit and shift all bits back through
    - ROR A     
      INX
      CPX #$08
      BNE -
    STA OBJECT_ALLOCATION

    LDA VAR_A        ; multiply slot number by 8 to get object data offset
    ASL A
    ASL A
    ASL A
    
    ADC #OBJECT_DATA ; add object data address
    
    STA PTR_B        ; low byte of object data slot address 
    LDA #$00
    STA PTR_B + 1    ; high byte of data slot address
    
    LDY #$00
    LDA (PTR_A), y   ; low byte of tick routine
    STA (PTR_B), y
    INY
    LDA (PTR_A), y   ; high byte of tick routine
    STA (PTR_B), y

+ RTS    
    
LoadAnimationFrame:
  ;; PTR_B holds the animation frame to load
  ;; X holds the OAM buffer offset 
  ;; VAR_A holds the x position
  ;; VAR_B holds the y position
  LDY #$00
  LDA (PTR_B), y    ; frame size in sprites
  STA VAR_C
  - INY
    LDA (PTR_B), y    ; sprite y offset 
    CLC
    ADC VAR_B         ; add y position
    STA BUFFER_OAM, x 
    INX
    
    INY
    LDA (PTR_B), y    ; tile number
    STA BUFFER_OAM, x
    INX
    
    INY
    LDA (PTR_B), y    ; attributes
    STA BUFFER_OAM, x 
    INX
    
    INY
    LDA (PTR_B), y    ; sprite x offset
    CLC
    ADC VAR_A         ; add x position
    STA BUFFER_OAM, x
    INX

    DEC VAR_C
    LDA VAR_C
    CMP #$00
    BNE -

+ RTS

ReadInput:
  ;; While the strobe bit is set, buttons will be continuously reloaded.
  ;; This means that reading from $4016 (JOYPAD_PORT1) will only return the state of the first button.
  ;; By storing 0 into JOYPAD_PORT1, the strobe bit is cleared and the reloading stops.
  ;; This allows all 8 buttons (newly reloaded) to be read from JOYPAD_PORT1.
  LDA #$01
  STA JOYPAD_PORT1  ; Set strobe bit
  STA JOYPAD1_STATE ; Clear previous input, initialize the ring buffer
  LSR A             ; Set A to 0
  STA JOYPAD_PORT1   ; Clear strobe bit
  - LDA JOYPAD_PORT1  ; The LSB of $4016 (JOYPAD_PORT1) contains the state of the current button.
                      ; Reading this address will shift in the next button state in the order ABsSUDLR.
    LSR A             ; Shift the button status into carry
    ROL JOYPAD1_STATE ; Shift carry into JOYPAD1_STATE
    BCC -             ; Stop when leading bit gets shifted out
  
  LDA JOYPAD1_STATE   ; Test if A button is held
  AND #BUTTON_A      
  BNE +               ; If A is not pressed clear timer and skip to B check
    STA JOYPAD1_TIMER_A   
  JMP ++            
+   ;; DEBUG screen scrolling right
    LDX SCROLL_X
    CPX #$FF
    BNE +
      INC SCREEN_X
      LSR BUFFER_PPU_CTRL
      LDA SCREEN_X
      LSR A
      ROL BUFFER_PPU_CTRL
  + INC SCROLL_X
    LDA #$01
    STA SCROLL_DIRECTION

    LDX JOYPAD1_TIMER_A ; If A is held increment the hold timer
    CPX #$FF            ; Prevent timer from rolling over
    BEQ ++
      INC JOYPAD1_TIMER_A   

++ LDA JOYPAD1_STATE   ; Test if B button is held
  AND #BUTTON_B      
  BNE +               ; If B is not pressed, clear timer and skip to end
    STA JOYPAD1_TIMER_B   
  JMP ++
  + ;; DEBUG screen scrolling
    LDX SCROLL_X
    CPX #$00
    BNE +
      DEC SCREEN_X
      LSR BUFFER_PPU_CTRL
      LDA SCREEN_X
      LSR A
      ROL BUFFER_PPU_CTRL
  + DEC SCROLL_X
    LDA #$00
    STA SCROLL_DIRECTION

    LDX JOYPAD1_TIMER_B ; If B is held increment the hold timer
    CPX #$FF            ; Prevent timer from rolling over
    BEQ ++
      INC JOYPAD1_TIMER_B   
  
++ RTS

  
PlayerObjectTick:
  ;; X holds object data offset
    
    TXA       ; data offset
    TAY       ; data offset
    ASL A
    ASL A
    PHA       ; OAM offset
     
    INY
    INY       ; x position
    LDA OBJECT_DATA, y 
    STA VAR_A 
    
    INY       ; y position
    LDA OBJECT_DATA, y 
    STA VAR_B 
    
    INY       ; animation state
    INY       ; animation timer

    LDA JOYPAD1_STATE
    AND #(BUTTON_LEFT | BUTTON_RIGHT | BUTTON_UP | BUTTON_DOWN)
    ;; load animation state into X
    BNE +
      ;; no directional buttons pressed
      LDX #$00           ; clear animation timer
      STX OBJECT_DATA, y
    JMP ++
    + ;; any directional buttons pressed
      LDX OBJECT_DATA, y ; increment animation timer
      INX                
      CPX #$10           ; roll over timer
      BMI +               
      LDX #$00           
    + STX OBJECT_DATA, y 
      TXA                ; put animation timer in A
      LDX #%00000010     ; walk 1 for 8 frames
      CMP #$08          
      BMI ++
      LDX #%00000100     ; walk 2 for 8 frames        
 ++ DEY                ; animation state
    STX VAR_C          ; walking state
    LDA OBJECT_DATA, y ; animation state 
    AND #%00000001     ; save facing flag
    ORA VAR_C          ; set walking state
    STA OBJECT_DATA, y 

    ;; handle inputs
    LDA JOYPAD1_STATE
    ROR A     
    BCC +        ; right button pressed
      PHA
      LDA VAR_A    ; player x position
      CMP #$80
      BPL ++       ; character is on left side
        INC VAR_A  ; move player right
      JMP +++
   ++   LDX SCROLL_X ; character is centered
        CPX #$FF     ; scroll screen right
        BNE ++
          INC SCREEN_X
          LSR BUFFER_PPU_CTRL
          LDA SCREEN_X
          LSR A
          ROL BUFFER_PPU_CTRL
     ++ INC SCROLL_X
        LDA #$01
        STA SCROLL_DIRECTION
  +++ LDA OBJECT_DATA, y
      AND #%11111110     ; clear facing left flag
      STA OBJECT_DATA, y
      PLA 
  
  + ROR A
    BCC +                ; left button pressed
      PHA
      DEC VAR_A          ; player x position
      LDA OBJECT_DATA, y 
      ORA #%00000001     ; set facing left flag
      STA OBJECT_DATA, y
      PLA
  
  + ROR A
    BCC +              ; down button pressed
    INC VAR_B          ; player y position 
  
  + ROR A 
    BCC +              ; up button pressed
    DEC VAR_B          ; player y position
  
  + ;; Load animation data address
    LDA #<PlayerAnimations
    STA PTR_B
    LDA #>PlayerAnimations
    STA PTR_B + 1

    LDX OBJECT_DATA, y ; animation state
    TYA                ; save data offset
    PHA
    LDY #$00
    - CPX #$00
      BEQ +
      LDA (PTR_B), y     ; animation sprite size
      ASL A              ; offset by *4 
      ASL A
      SEC
      ADC PTR_B          ; add PTR_B + 1 to find next frame
      BCC ++             ; handle carry
        INC PTR_B + 1
   ++ STA PTR_B          
      DEX
      JMP -
    + 
    PLA                ; restore data offset
    TAY
    DEY                ; y position offset

    ;; store back new position
    LDA VAR_B        
    STA OBJECT_DATA, y ; y position
    DEY                ; x position offset
    LDA VAR_A
    STA OBJECT_DATA, y ; x position
    
    PLA
    TAX ; store OAM offset in X
    JSR LoadAnimationFrame
    RTS

DummyObjectTick:
    NOP
    RTS

;; DATA TABLES

.include nametable.asm

Objects:
PlayerObject:
  .dw PlayerObjectTick
DummyObject:
  .dw DummyObjectTick


PaletteData:
BackgroundPaletteData:
  .db $0F,$30,$10,$00, $0F,$36,$17,$08, $0F,$39,$3A,$3B ,$0F,$3D,$3E,$0F  ; background palette data
SpritePaletteData:
  .db $21,$16,$27,$18, $0F,$36,$17,$08, $0F,$1C,$15,$14, $0F,$02,$38,$3C  ; sprite palette data
  


;; SPRITE DATA
  ;; Each sprite needs 4 bytes of data for its position and tile information in this order:
  ;;
  ;; 0 | Y Position  | $00 = top of screen, $EF = bottom of screen
  ;; 1 | Tile Number | 0 - 254, tile number of top of sprite. Lowest bit is tile bank.					
  ;; 2 | Attributes  | Holds color and display info:
  ;;                   76543210
  ;;                   ||||||||
  ;;                   ||||||++- Color Palette of sprite. Choose which set of 4 colors to use (4-7)
  ;;                   |||+++--- Unimplemented
  ;;                   ||+------ Priority (0: in front of background; 1: behind background)
  ;;                   |+------- Flip sprite horizontally
  ;;                   +-------- Flip sprite vertically
  ;; 3 | X Position  | $00 = left, $F9 = right
  ;;
  ;; These 4 bytes repeat 64 times (one set per sprite) to fill the 256 bytes of sprite memory. 
  ;; To edit sprite 0, change bytes $0200-0203, Sprite 1 is $0204-0207, etc.

Animations:
PlayerAnimations:
PlayerSmallStandRight:
  .db $04
  .db $00,$3A,%00000000,$00
  .db $00,$37,%00000000,$08
  .db $08,$4F,%00000000,$00
  .db $08,$4F,%01000000,$08

PlayerSmallStandLeft:
  .db $04
  .db $00,$37,%01000000,$00
  .db $00,$3A,%01000000,$08
  .db $08,$4F,%00000000,$00
  .db $08,$4F,%01000000,$08

PlayerSmallWalkRight1:
  .db $04
  .db $00,$36,%00000000,$00
  .db $00,$37,%00000000,$08
  .db $08,$38,%00000000,$00
  .db $08,$39,%00000000,$08

PlayerSmallWalkLeft1:
  .db $04
  .db $00,$37,%01000000,$00
  .db $00,$36,%01000000,$08
  .db $08,$39,%01000000,$00
  .db $08,$38,%01000000,$08

PlayerSmallWalkRight2:
  .db $04
  .db $00,$3A,%00000000,$00
  .db $00,$37,%00000000,$08
  .db $08,$3B,%00000000,$00
  .db $08,$3C,%00000000,$08

PlayerSmallWalkLeft2:
  .db $04
  .db $00,$37,%01000000,$00
  .db $00,$3A,%01000000,$08
  .db $08,$3C,%01000000,$00
  .db $08,$3B,%01000000,$08

;; debug
  .db $02
  .db $00,$75,%00000000,$00
  .db $00,$74,%00000000,$08

  .db $03
  .db $00,$75,%00000000,$00
  .db $00,$74,%00000000,$08
  .db $00,$74,%00000000,$10

  .db $04
  .db $00,$75,%00000000,$00
  .db $00,$74,%00000000,$08
  .db $00,$74,%00000000,$10
  .db $00,$74,%00000000,$18

  .db $02
  .db $00,$74,%00000000,$00
  .db $00,$84,%00000000,$08
  
  .db $01
  .db $00,$84,%00000000,$00
  
  .db $02
  .db $00,$84,%00000000,$00
  .db $00,$74,%00000000,$08

  .db $03
  .db $00,$84,%00000000,$00
  .db $00,$74,%00000000,$08
  .db $00,$74,%00000000,$10

  .db $04
  .db $00,$84,%00000000,$00
  .db $00,$74,%00000000,$08
  .db $00,$74,%00000000,$10
  .db $00,$74,%00000000,$18

  .db $03
  .db $00,$84,%00000000,$00
  .db $00,$74,%00000000,$08
  .db $00,$75,%00000000,$10

  .db $02
  .db $00,$84,%00000000,$00
  .db $00,$75,%00000000,$08
  
  .db $03
  .db $00,$84,%00000000,$00
  .db $00,$75,%00000000,$08
  .db $00,$74,%00000000,$10
  
;; INTERRUPT VECTORS 
  ;; There are 3 times when the NES processor will interrupt your code and jump to a new location. 
  ;; These vectors are held in PRG ROM and tell the processor where to go when that happens.
  .org $FFFA
  .dw NMI   ; NMI Vector: happens once per frame when enabled. The PPU tells the processor it is starting vblank and is available for graphics updates
  .dw RESET ; RESET Vector: happens every time the NES starts up, or the reset button is pressed.
  .dw $0    ; IRQ Vector: Triggered from some mapper chips or audio interrupts
  
  
;; GRAPHICS DATA
.incbin "graphics.chr"
  