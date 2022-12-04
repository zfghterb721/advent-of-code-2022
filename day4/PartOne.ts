import { readFileSync } from "fs";

const inputData = readFileSync("data.txt", "utf8");

const splitByChar = (c:string) => (str:string):Array<any> => str.split(c);

const parsed = (splitByChar("\n")(inputData)).map(splitByChar(",")).map((a) => a.map(splitByChar("-"))).map((a) => a.map((b) => b.map((c) => parseInt(c))))

const compareRange = (a:number[]) => (b:number[]) => {
  if(!a[0] || !b[0]) return false;
  if(!a[1] || !b[1]) return false;
  if ((a[0] >= b[0]) && (a[1]<=b[1])) return true;
  return false;
}

const compared = parsed.map(
  (a) => compareRange(a[0])(a[1]) || compareRange(a[1])(a[0])
);

console.log(compared)
