import Copyable from "stopping-time/model/Copyable";

export default class Award implements Copyable<Award> {
  public constructor(value?: number, probability?: number) {
    if (value) {
      this.value_ = value;
    }
    if (probability) {
      this.probability_ = probability;
    }
  }

  private value_: number = 1;
  private probability_: number = 0;

  public get value(): number {
    return this.value_;
  }

  public get probability(): number {
    return this.probability_;
  }

  public setValue(value: number): Award {
    let ret = this.copy();
    ret.value_ = value;
    return ret;
  }

  public setProbability(probability: number): Award {
    let ret = this.copy();
    ret.probability_ = probability;
    return ret;
  }

  public copy(): Award {
    return new Award(this.value, this.probability);
  }

}