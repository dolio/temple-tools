
module Temple.Object.Field.Extra where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

-- These seem to be general flags that were probably added later.
data ExtraField
  = TotalNormal
  | TransientBegin
  | RenderColor
  | RenderColors
  | RenderPalette
  | RenderScale
  | RenderAlpha
  | RenderX
  | RenderY
  | RenderWidth
  | RenderHeight
  | Palette
  | Color
  | Colors
  | RenderFlags
  | TempId
  | LightHandle
  | OverlayLightHandles
  | InternalFlags
  | FindNode
  | AnimationHandle
  | GrappleState
  | TransientEnd
  | Type
  | PrototypeHandle
  deriving (Bounded, Eq, Ord, Show)

instance Enum ExtraField where
  fromEnum = \case
    TotalNormal         -> 405
    TransientBegin      -> 406
    RenderColor         -> 407
    RenderColors        -> 408
    RenderPalette       -> 409
    RenderScale         -> 410
    RenderAlpha         -> 411
    RenderX             -> 412
    RenderY             -> 413
    RenderWidth         -> 414
    RenderHeight        -> 415
    Palette             -> 416
    Color               -> 417
    Colors              -> 418
    RenderFlags         -> 419
    TempId              -> 420
    LightHandle         -> 421
    OverlayLightHandles -> 422
    InternalFlags       -> 423
    FindNode            -> 424
    AnimationHandle     -> 425
    GrappleState        -> 426
    TransientEnd        -> 427
    Type                -> 428
    PrototypeHandle     -> 429

  toEnum = \case
    405 -> TotalNormal
    406 -> TransientBegin
    407 -> RenderColor
    408 -> RenderColors
    409 -> RenderPalette
    410 -> RenderScale
    411 -> RenderAlpha
    412 -> RenderX
    413 -> RenderY
    414 -> RenderWidth
    415 -> RenderHeight
    416 -> Palette
    417 -> Color
    418 -> Colors
    419 -> RenderFlags
    420 -> TempId
    421 -> LightHandle
    422 -> OverlayLightHandles
    423 -> InternalFlags
    424 -> FindNode
    425 -> AnimationHandle
    426 -> GrappleState
    427 -> TransientEnd
    428 -> Type
    429 -> PrototypeHandle
    n -> error $ "toEnum @ExtraField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field ExtraField where
  fieldName = \case
    TotalNormal         -> "obj_f_total_normal"
    TransientBegin      -> "obj_f_transient_begin"
    RenderColor         -> "obj_f_render_color"
    RenderColors        -> "obj_f_render_colors"
    RenderPalette       -> "obj_f_render_palette"
    RenderScale         -> "obj_f_render_scale"
    RenderAlpha         -> "obj_f_render_alpha"
    RenderX             -> "obj_f_render_x"
    RenderY             -> "obj_f_render_y"
    RenderWidth         -> "obj_f_render_width"
    RenderHeight        -> "obj_f_render_height"
    Palette             -> "obj_f_palette"
    Color               -> "obj_f_color"
    Colors              -> "obj_f_colors"
    RenderFlags         -> "obj_f_render_flags"
    TempId              -> "obj_f_temp_id"
    LightHandle         -> "obj_f_light_handle"
    OverlayLightHandles -> "obj_f_overlay_light_handles"
    InternalFlags       -> "obj_f_internal_flags"
    FindNode            -> "obj_f_find_node"
    AnimationHandle     -> "obj_f_animation_handle"
    GrappleState        -> "obj_f_grapple_state"
    TransientEnd        -> "obj_f_transient_end"
    Type                -> "obj_f_type"
    PrototypeHandle     -> "obj_f_prototype_handle"

  fieldType = \case
    TotalNormal -> NoneF
    TransientBegin -> BeginF
    RenderColor -> W32F
    RenderColors -> W32F
    RenderPalette -> W32F
    RenderScale -> W32F
    RenderAlpha -> AbilityArrF
    RenderX -> W32F
    RenderY -> W32F
    RenderWidth -> W32F
    RenderHeight -> W32F
    Palette -> W32F
    Color -> W32F
    Colors -> W32F
    RenderFlags -> W32F
    TempId -> W32F
    LightHandle -> W32F
    OverlayLightHandles -> W32ArrF
    InternalFlags -> W32F
    FindNode -> W32F
    AnimationHandle -> W32F
    GrappleState -> W32F
    TransientEnd -> EndF
    Type -> W32F
    PrototypeHandle -> ObjF

  isPadding _ = False
