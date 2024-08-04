package com.sxyangsuper.exunifier.common.asserts;

import com.sxyangsuper.exunifier.common.BaseException;

public interface IAsserts<E extends BaseException> extends IArrayAsserts<E>, IBaseAsserts<E>, ICharSequenceAsserts<E>, IComparisonAsserts<E>, IFunctionalAsserts<E>, IIterableAsserts<E>, IThrowAsserts<E> {
}
