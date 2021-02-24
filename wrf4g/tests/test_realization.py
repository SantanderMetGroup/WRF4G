from datetime import datetime
from wrf4g.core import Realization


def test_realization():
    rea = Realization()
    rea_id = 15
    chunk_start_date = datetime(2011, 8, 28, 12)
    chunk_end_date = datetime(2011, 8, 30, 0)
    chunk_id = 1
    ch = rea.check_db(rea_id, chunk_start_date, chunk_end_date, chunk_id)
    print(ch)


if __name__ == "__main__":
    test_realization()