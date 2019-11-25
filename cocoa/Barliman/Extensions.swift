//
//  Extensions.swift
//  Barliman
//
//  Created by Adam Nemecek on 7/31/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Foundation

extension Sequence {
    func find(_ pred: (Iterator.Element) -> Bool) -> Iterator.Element? {
        for e in self where pred(e) {
            return e
        }
        return nil
    }
}
