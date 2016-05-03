r = {"Christ people. This is just shit.",
     "Anybody who thinks that code is a) legible, b) efficient or c) safe is just incompetent and out to lunch.",
     "It looks bad, and there is no reason for it.",
     "Really. Give me *one* reason why it was written in that idiotic way.",
     "I really see no reason for this kind of complete idiotic crap.",
     "I want to make it clear to *everybody* that code like this is completly unacceptable.",
     "Anybody who thinks code lie that is 'safe' and 'secure' is so far out to lunch that it's not even funny.",
     "This crap is an unreadable mess that no sane person will ever really understand.",
     "Get rid of it. And I don't *ever* want to see that shit again.",
     "Christ, people. Learn C, instead of just stringing random characters together until it compiles (with warnings).",
     "There are arguments for this kind of code, but they are from weak minds.",
     "Please people. When I see these kinds of obviously bogus code problems, that just makes me very upset.",
     "I really am very tired indeed of these 'trivially obvious improvements' that are buggy and actually introduce whole new ways to write buggy code.",
     "It's clearly total and utter CRAP.",
     "You've shown yourself to not be competent in this issue, so I'll fix it directly and immediately myself",
     "I'm angry, because the whole thing is so _horribly_ wrong.",
     "The whole thing is incredibly broken shit.",
     "Fix your f*cking code, because it is obviously broken. And fix your approach to programming."} 

function message(channel, text)
    chop = string.gsub(text,"-","")
    m = string.match(chop,"pullrequest")
    if(m ~= nil) then
        karl.send_message(channel, r[math.random(1, 18)])
    end
end

