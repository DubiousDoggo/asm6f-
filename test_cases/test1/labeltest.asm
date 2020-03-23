

.org $00

LABEL_NO_COLON
    NOP

LABEL_WITH_COLON:
    nop

label_with_comment ; hello
    NOP

ANOTHER_LABEL: ; colon and comment
    JMP + ; foreward jump
-   NOP
+   NOP
    JMP -
-:  NOP
    JMP -
    JMP +

LABEL2:
+:  jmp -
    jmp LABEL_WITH_COLON

@local_label
    NOP
@local_label2:
    nop
    jmp @local_label

LABEL3
    jmp @local_label
    nop
@local_label
    nop    

-directional:
    nop
    jmp +directional
    jmp -directional
+directional:
    nop