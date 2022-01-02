from member import Member

class Element(Member):
    TYPE = 'Element'

    def __init__(self, id : int, nodes: list, pid: int, mid: int, label: str = None):
        super().__init__(id, label)


