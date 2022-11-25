const fs = require("fs")

const inPath = "./test/Classless/Arbitrary.purs"
const outPath = "./README.md"

const parse = (str) => {
    const lines = str.split("\n")

    const out = []
    let cur = []
    let isComment = true

    const swap = () => {
        out.push({ type: isComment ? "comment" : "code", value: cur })
        cur = []
        isComment = !isComment
    }

    for (const line of lines) {
        if (line.startsWith("--")) {
            if (!isComment) swap()
            cur.push(line.slice(3))
        } else {
            if (isComment) swap()
            cur.push(line)
        }
    }

    swap()

    return out
}

const print = (data) => {
    let lines = ""
    for (const item of data) {
        switch (item.type) {
            case "comment":
                lines += item.value.join("\n")
                break;
            case "code":
                lines += "\n```hs\n" + item.value.join("\n").trim() + "\n```\n"
                break;
        }
    }

    return lines
}


const main = () => {
    const lines = fs.readFileSync(inPath).toString()
    const data = parse(lines)
    const str = print(data)
    
    fs.writeFileSync(outPath, str)
}


main()