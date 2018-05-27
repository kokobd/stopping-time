export declare function do_some_thing(obj: MyObj): void;

export declare interface MyObj {
  msg: string;
}

export declare type D<T> = "nullary" | IUnary<T>

type IUnary<T> = number;