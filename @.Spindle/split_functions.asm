.function addPart(parts, start, end) {
    .var startEnd = List().add(start, end)
    .eval parts.add(startEnd)
    .return parts
}

.function createParts(start1, end1) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .return parts
}

.function createParts(start1, end1, start2, end2) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .eval parts = addPart(parts, start2, end2)
    .return parts
}

.function createParts(start1, end1, start2, end2, start3, end3) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .eval parts = addPart(parts, start2, end2)
    .eval parts = addPart(parts, start3, end3)
    .return parts
}

.function createParts(start1, end1, start2, end2, start3, end3, start4, end4) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .eval parts = addPart(parts, start2, end2)
    .eval parts = addPart(parts, start3, end3)
    .eval parts = addPart(parts, start4, end4)
    .return parts
}

.function createParts(start1, end1, start2, end2, start3, end3, start4, end4, start5, end5) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .eval parts = addPart(parts, start2, end2)
    .eval parts = addPart(parts, start3, end3)
    .eval parts = addPart(parts, start4, end4)
    .eval parts = addPart(parts, start5, end5)
    .return parts
}

.function createParts(start1, end1, start2, end2, start3, end3, start4, end4, start5, end5, start6, end6) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .eval parts = addPart(parts, start2, end2)
    .eval parts = addPart(parts, start3, end3)
    .eval parts = addPart(parts, start4, end4)
    .eval parts = addPart(parts, start5, end5)
    .eval parts = addPart(parts, start6, end6)
    .return parts
}

.function createParts(start1, end1, start2, end2, start3, end3, start4, end4, start5, end5, start6, end6, start7, end7) {
    .var parts = List()
    .eval parts = addPart(parts, start1, end1)
    .eval parts = addPart(parts, start2, end2)
    .eval parts = addPart(parts, start3, end3)
    .eval parts = addPart(parts, start4, end4)
    .eval parts = addPart(parts, start5, end5)
    .eval parts = addPart(parts, start6, end6)
    .eval parts = addPart(parts, start7, end7)
    .return parts
}

.function createSplitEfoCmdFile(fn, effectName, efoHeaderSize, loadAddress, parts) {
    .var myFile = createFile(fn)
    .var ifName = ""+effectName+".efo"
    .for (var i=0; i<parts.size(); i++) {
        .var part = parts.get(i)
        .var partOffset = efoHeaderSize + (part.get(0) - loadAddress) 
        .var partSize = part.get(1) - part.get(0)
        .var ofName = ""+effectName+"_part"+(i+1)+".bin"
        .if (i == 0) {
            .eval partOffset = 0
            .eval partSize = efoHeaderSize + (part.get(1) - loadAddress)
            .eval ofName = ""+effectName+"_part"+(i+1)+".efo"
        }
        .eval myFile.writeln("dd if="+ifName+" of="+ofName+" bs=1 skip="+partOffset+" count="+partSize)
    }
}
