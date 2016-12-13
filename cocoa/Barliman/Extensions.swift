//
//  Extensions.swift
//  Barliman
//
//  Created by Adam Nemecek on 7/31/16.
//  Copyright © 2016 William E. Byrd. All rights reserved.
//

import Foundation

extension Sequence {
    func find(_ pred: (Iterator.Element) -> Bool) -> Iterator.Element? {
        for e in self where pred(e) {
            return e
        }
        return nil
    }
}
