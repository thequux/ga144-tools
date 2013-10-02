# -*- coffee-tab-width: 4 -*-

class CodeFormatter
    tag2class:
        1: "exec"
        2: "exec"
        3: "define"
        4: "compile"
        5: "compile"
        6: "compile"
        7: "exec"
        8: "exec"
        9: "comment"
        10: "comment"
        11: "comment"
        12: "variable"
        13: "feedback"
        14: "display"
        15: "comment"

    displayWord: null

    formatWord: (word, params, highlight) ->
        [tag, name, value] = word
        res = $("<span>").addClass(@tag2class[tag]).text(name)
        if highlight
            res.addClass("highlight")
        if value?
            res.attr("data-value", value)

        params.implicitNL = (tag == 3)

        # figure out what action to take, if any
        if tag in [1,4,7]
            res.addClass("active").click ->
                window.CodeBrowser.showDefs(name)
        else if tag in [3,12]
            res.addClass("active").click ->
                window.CodeBrowser.showRefs(name)
            
        if tag == 14
            if name of @displayWord
               return [res, @displayWord[name]]
            res.addClass("note")
            params.suppressSpace = true
            return [res, null]
        else
            return [res, null]

    defParams: () ->
        suppressSpace: false
        implictNL: false # add a nl beforehand, unless noNL
        noNL: false # don't insert newline, even if it would be implicit
        inserter: @defaultInserter
        _reset: () ->
            @implicitNL = no
            @noNL = no
            @suppressSpace = no
            
    defaultInserter: (elem, word, params) ->
        if params.implicitNL and !params.noNL
            elem.append("<br>")
        else if !params.suppressSpace
            # If we added a newline, no space.
            elem.append(" ")
                
        elem.append(word)
        params._reset()

    formatCode: (block, target, highlight) ->
        result = $()
        params = @defParams()
        params.suppressSpace = true
        
        if typeof block == "number"
            block = OkadSrc[block]
       
        target.empty()
        for word, wn in block
            [word, inserter] = @formatWord(word, params, wn == highlight)
            inserter ?= @defaultInserter
            inserter.call(this, target, word, params)
        return

initDisplayWords = () ->
    justPrint = (prespace) ->
        (dest, word, params) ->
            if prespace and !params.suppressSpace
                dest.append(" ")
            dest.append(word)
            params.noNL = yes
            params.suppressSpace = yes
        
    CodeFormatter::displayWord =
        ",": (dest, word, params) ->
            dest.append(word, "<br>")
            params.suppressSpace = true
            params.noNL = yes
        "*": justPrint(no)
        "cr": (dest, word, params) ->
            dest.append(" ", word, "<br>")
            params.noNL = yes
        "br": (dest, word, params) ->
            dest.append(" ", word, "<br>")
        "indent": (dest, word, params) ->
            dest.append(word, "<br>", "<span style='width: 8ex'>&nbsp;</span>")
        ".": justPrint yes
        "..": justPrint yes
        "...": justPrint yes
initDisplayWords()

class CodeBrowser
    startblock: 18
    constructor: (@src) ->
        # compute indices (maybe. Depends on how long a lookup takes)
        @formatter = new CodeFormatter()
        @curBlock = 18
        $('#next-page').click =>
            @list(@curBlock + 2)
        $('#prev-page').click =>
            @list(@curBlock - 2)
        
    forAllWords: (fn) ->
        for block, bn in @src
            if bn < @startblock
                continue
            for word,wn in block
                fn(bn, wn, word)
    findWords: (fn) ->
        res = []
        @forAllWords (bn, wn, word) ->
            if fn(bn, wn, word)
                res.push([bn, wn])
        res

    showLinks: (title, refList) ->
        refDom = $("<div class='refbox'>").append(
            $("<span class='closer'>X</span>").click(-> refDom.remove()),
            title)
        for ref in refList
            do (ref) =>
                 refDom.append(" ",
                               $("<a class='hyper'>")
                                 .text(ref[0])
                                 .click => @list(ref[0], ref[1]))
        $('#refs').append(refDom)
            

    showRefs: (word) -> @showLinks($("<span>").text("Refs to #{word} "),
                                   @wordRefs(word))
    showDefs: (word) -> @showLinks($("<span>").text("Defs of #{word} "),
                                   @wordDefs(word))
    wordDefs: (target) ->
        @findWords (bn, wn, word) ->
            if word[1] != target
                return false
            CodeFormatter::tag2class[word[0]] in ["define", "variable"]
    wordRefs: (target) ->
        @findWords (bn, wn, word) ->
            if word[1] != target
                return false
            CodeFormatter::tag2class[word[0]] in ["exec", "compile"]
    list: (block, word, skipHist) ->
        @curBlock = block
        setPage = () =>
            onsub = (e) =>
                console.log(arguments)
                @list(e.target.block.value - 0)
                false
            $('#curpage').empty().append(
                $("<form>").submit(onsub).append(
                    inf = $("<input type='text' name='block'>").text(@curBlock)))
            inf.focus()
        $('#curpage').empty().append($('<span>').text(block).click(setPage))
        unless skipHist
            target = "##{block}"
            if word != undefined
                target += ".#{word}"
            history.pushState({block: block, word: word}, "", target)
        blockHI = unless block % 2 then word
        shadowHI = if block % 2 then word
        base = block - (block % 2)
        @formatter.formatCode(base, $('#code'), blockHI)
        @formatter.formatCode(base + 1, $('#shadow'), shadowHI)

window.onpopstate = (event) =>
    if event.state?
        window.CodeBrowser.list(event.state.block, event.state.word, true)

$ ->
    window.CodeBrowser = new CodeBrowser(OkadSrc)
    if location.hash
        window.CodeBrowser.list(location.hash.substr(1) - '0')
    else
        window.CodeBrowser.list(18)
