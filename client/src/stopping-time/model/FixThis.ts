/**
 * The semantic of 'this' keyword in JavaScript
 * and TypeScript is incorrect. Calling this
 * function on any object will fix that.(only apply
 * to the methods in the class of that object,
 * not including the methods inherited from
 * parents
 * @param obj the object in which 'this' keyword
 * is fixed
 */
export default function fixThis(obj: any): void {
  let proto = Object.getPrototypeOf(obj);
  for (let key of Object.getOwnPropertyNames(proto)) {
    if (typeof(obj[key]) === "function") {
      obj[key] = obj[key].bind(obj);
    }
  }
}